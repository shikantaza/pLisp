/**
  Copyright 2011-2022 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

  This file is part of pLisp.

  pLisp is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  pLisp is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with pLisp.  If not, see <http://www.gnu.org/licenses/>.
**/

#include "clang/Basic/DiagnosticOptions.h"
#include "clang/CodeGen/CodeGenAction.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Tool.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
//#include "llvm/ADT/SmallString.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
//#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
//#include "llvm/Support/ManagedStatic.h"
//#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
//#include "llvm/Support/raw_ostream.h"
//#include "llvm/Target/TargetMachine.h"

#include "llvm/Linker/Linker.h"

#include <iostream>

typedef uintptr_t OBJECT_PTR;

typedef OBJECT_PTR (*nativefn)(OBJECT_PTR, ...);

using namespace clang;
using namespace clang::driver;

using namespace std;

static llvm::LLVMContext theContext;

std::string GetExecutablePath(const char *Argv0, void *MainAddr) {
  return llvm::sys::fs::getMainExecutable(Argv0, MainAddr);
}

namespace llvm {
namespace orc {

class SimpleJIT {
private:
  ExecutionSession ES;
  std::shared_ptr<SymbolResolver> Resolver;
  std::unique_ptr<TargetMachine> TM;
  const DataLayout DL;
  LegacyRTDyldObjectLinkingLayer ObjectLayer;
  LegacyIRCompileLayer<decltype(ObjectLayer), SimpleCompiler> CompileLayer;

public:
  SimpleJIT()
      : Resolver(createLegacyLookupResolver(
            ES,
            //[this](const std::string &Name) -> JITSymbol {
            [this](const llvm::StringRef Name) -> JITSymbol {
              if (auto Sym = CompileLayer.findSymbol(Name.data(), false))
                return Sym;
              else if (auto Err = Sym.takeError())
                return std::move(Err);
              if (auto SymAddr =
                  RTDyldMemoryManager::getSymbolAddressInProcess(Name.data()))
                return JITSymbol(SymAddr, JITSymbolFlags::Exported);
              return nullptr;
            },
            [](Error Err) { cantFail(std::move(Err), "lookupFlags failed"); })),
        TM(EngineBuilder().selectTarget()), DL(TM->createDataLayout()),
        ObjectLayer(ES,
                    [this](VModuleKey) {
                      return LegacyRTDyldObjectLinkingLayer::Resources{
                          std::make_shared<SectionMemoryManager>(), Resolver};
                    }),
        CompileLayer(ObjectLayer, SimpleCompiler(*TM)) {
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }

  const TargetMachine &getTargetMachine() const { return *TM; }

  VModuleKey addModule(std::unique_ptr<Module> M) {
    // Add the module to the JIT with a new VModuleKey.
    auto K = ES.allocateVModule();
    cantFail(CompileLayer.addModule(K, std::move(M)));
    return K;
  }

  JITSymbol findSymbol(const StringRef &Name) {
    std::string MangledName;
    raw_string_ostream MangledNameStream(MangledName);
    Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    return CompileLayer.findSymbol(MangledNameStream.str(), true);
  }

  JITTargetAddress getSymbolAddress(const StringRef &Name) {
    return cantFail(findSymbol(Name).getAddress());
  }

  void removeModule(VModuleKey K) {
    cantFail(CompileLayer.removeModule(K));
  }
};

} // end namespace orc
} // end namespace llvm


llvm::orc::SimpleJIT *theJITObject;

extern "C" void initializeJIT()
{
  theJITObject = new llvm::orc::SimpleJIT();
}

llvm::orc::VModuleKey addModuleToJIT(std::unique_ptr<llvm::Module> module)
{
  if(!theJITObject)
    initializeJIT();

  auto K = theJITObject->addModule(std::move(module));

  return K;
}

extern "C" nativefn get_function(void *state, const char *fn_name)
{
  return (nativefn)theJITObject->getSymbolAddress(fn_name);
}

extern "C" void cleanupJIT(void *state)
{
  //TODO: code to remove modules from JIT object
  llvm::llvm_shutdown();
}

std::unique_ptr<llvm::Module> convert_file_to_module(const char * c_file_name) {

  // This just needs to be some symbol in the binary; C++ doesn't
  // allow taking the address of ::main however.
  void *MainAddr = (void*) (intptr_t) GetExecutablePath;

  //std::string Path = GetExecutablePath(argv[0], MainAddr);
  std::string Path = GetExecutablePath("a.out", MainAddr);

  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
  TextDiagnosticPrinter *DiagClient =
    new TextDiagnosticPrinter(llvm::errs(), &*DiagOpts);

  IntrusiveRefCntPtr<DiagnosticIDs> DiagID(new DiagnosticIDs());
  DiagnosticsEngine Diags(DiagID, &*DiagOpts, DiagClient);

  const std::string TripleStr = llvm::sys::getProcessTriple();
  llvm::Triple T(TripleStr);

  // Use ELF on Windows-32 and MingW for now.
#ifndef CLANG_INTERPRETER_COFF_FORMAT
  if (T.isOSBinFormatCOFF())
    T.setObjectFormat(llvm::Triple::ELF);
#endif

  Driver TheDriver(Path, T.str(), Diags);
  TheDriver.setTitle("clang interpreter");
  TheDriver.setCheckInputsExist(false);

  SmallVector<const char *, 16> Args;
  Args.push_back("a.out");
  Args.push_back(c_file_name);
  Args.push_back("-O2");
  Args.push_back("-Wno-return-type"); //to disable 'non-void function does not return a value' warning
  std::unique_ptr<Compilation> C(TheDriver.BuildCompilation(Args));
  if (!C)
    return 0;

  const driver::JobList &Jobs = C->getJobs();

  const driver::Command &Cmd = cast<driver::Command>(*Jobs.begin());

  // Initialize a compiler invocation object from the clang (-cc1) arguments.
  const llvm::opt::ArgStringList &CCArgs = Cmd.getArguments();
  std::unique_ptr<CompilerInvocation> CI(new CompilerInvocation);

  llvm::ArrayRef<const char *> ar(CCArgs.data(), CCArgs.size());
  
  // CompilerInvocation::CreateFromArgs(*CI,
  //                                    const_cast<const char **>(CCArgs.data()),
  //                                    const_cast<const char **>(CCArgs.data()) +
  //                                      CCArgs.size(),
  //                                    Diags);
  CompilerInvocation::CreateFromArgs(*CI,
                                     ar,
                                     Diags);

  
  // Create a compiler instance to handle the actual work.
  CompilerInstance Clang;
  Clang.setInvocation(std::move(CI));

  // Create the compilers actual diagnostics engine.
  Clang.createDiagnostics();
  if (!Clang.hasDiagnostics())
    return NULL;

  // Create and execute the frontend to generate an LLVM bitcode module.
  std::unique_ptr<CodeGenAction> Act(new EmitLLVMOnlyAction(&theContext));
  if (!Clang.ExecuteAction(*Act))
    return NULL;

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  return Act->takeModule();
}

std::unique_ptr<llvm::Module> compile_fns_internal(const char * c_source) {

  FILE *f = fopen("plisp_jit_temp.c", "w");
  fprintf(f, "%s", c_source);
  fclose(f);

  std::unique_ptr<llvm::Module> module = convert_file_to_module("plisp_jit_temp.c");

#ifdef _WIN64
  system("del plisp_jit_temp.c");
#else
  system("rm plisp_jit_temp.c");
#endif
  
  return module;
}

extern "C" void *compile_functions_from_string(const char *c_source)
{
  std::unique_ptr<llvm::Module> module = compile_fns_internal(c_source);

  addModuleToJIT(std::move(module));

  return NULL;
}
