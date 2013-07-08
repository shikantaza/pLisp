/**
  Copyright 2011-2013 Rajesh Jayaprakash <rajesh.jayaprakash@gmail.com>

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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "plisp.h"

#include "memory.h"

#include "rb/red_black_tree.h"

RAW_PTR *heap;
RAW_PTR free_list;
RAW_PTR last_segment;

#ifdef CUSTOM_BST
//do we need the black set at all?
struct node *black = NULL, *white = NULL, *grey = NULL;
#else
rb_red_blk_tree *white, *grey, *black;
#endif

extern OBJECT_PTR top_level_env, NIL;

extern BOOLEAN IS_CONS_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CLOSURE_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_MACRO_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_ARRAY_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_CONTINUATION_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_INTEGER_OBJECT(OBJECT_PTR);
extern BOOLEAN IS_FLOAT_OBJECT(OBJECT_PTR);

//forward declarations

void dealloc(RAW_PTR);

void insert_node(unsigned int, OBJECT_PTR);

#ifdef CUSTOM_BST

struct node *create_node(OBJECT_PTR);
struct node *put(struct node *, OBJECT_PTR);
void insert_node_orig(struct node **, struct node *);
void remove_node(struct node **, OBJECT_PTR);
void destroy(struct node **);
void print_tree(struct node *);
BOOLEAN is_set_empty(struct node *);
int get_size_of_tree(struct node *);
int get_height_of_tree(struct node *);
BOOLEAN value_exists(struct node **, OBJECT_PTR);

#else

void remove_node(rb_red_blk_tree *, OBJECT_PTR);
void destroy(rb_red_blk_tree *);
void print_tree(rb_red_blk_tree *);
BOOLEAN is_set_empty(rb_red_blk_tree *);
BOOLEAN value_exists(rb_red_blk_tree *, OBJECT_PTR);

#endif

void gc();
BOOLEAN is_dynamic_memory_object(OBJECT_PTR);
void build_grey_set();

int get_free_memory();

//end forward declarations

enum {WHITE, GREY, BLACK};

int initialize_memory()
{
  heap = (RAW_PTR *)malloc(HEAP_SIZE * sizeof(RAW_PTR));

  if(!heap)
  {
    fprintf(stderr, "Unable to create heap of size %d\n", HEAP_SIZE);
    return -1;
  }

  free_list = 0;
  heap[free_list] = HEAP_SIZE;
  heap[free_list + 1] = null;

  last_segment = 0;

  return 0;
}

//first-fit (use the first segment
//whose length is closest
//to the required size)
RAW_PTR object_alloc(int size, unsigned int tag)
{
  assert(size > 0 && size <= HEAP_SIZE);

  RAW_PTR it = free_list;
  RAW_PTR prev = null;

  RAW_PTR ret;

  while(it != null)
  {
    //if a segment would have less than four words
    //after allocating the required space,
    //it is not considered as it will not have
    //any usable space after the allocation
    if((heap[it] - (size + 1)) >= 3)
    {
      //the current segment will not have any
      //usable space after the allocation, 
      //so it will have to be
      //removed from the free list
      //[we allocate the entire segment
      //though the caller will not (and should not)
      //use the additional memory]
      if((heap[it] - (size + 1)) == 3)
      {
	//point the previous segment's 'next' to
	//the current segment's 'next
	//(i.e., remove the current segment
	//from the free list)
	if(prev != null)
	  heap[prev] = heap[it + 1]; 

        insert_node(WHITE, ((it+1) << OBJECT_SHIFT) + tag);

	return it + 1;
      }
      else
      {

	//minimum size allocated is two;
	//if we allow an allocation size of one,
	//we cannot use this segment after deallocation,
	//since a minimum of three words is required
	//for a segment (size, pointer to next segment,
	//and space for the actual data)

	int new_size;
	if(size == 1)
	  new_size = 2;
	else
	  new_size = size;

	//reduce the size of the segment (left over after 
	//allocating the new segment)
	heap[it] = heap[it] - new_size - 1;

	//set the size field of the newly created segment
	heap[it + heap[it]] = new_size + 1;

	ret = it + heap[it] + 1;

        insert_node(WHITE, (ret << OBJECT_SHIFT) + tag);

	//return the address of the new segment
	return ret;
      }
    }

    prev = it;
    it = heap[it + 1];
  }

  printf("Unable to allocate memory\n");
  return -1;
}

void dealloc(RAW_PTR ptr)
{
  assert(ptr >= 0 && ptr < HEAP_SIZE);

  //to avoid integer and float objects being incorrectly flagged
  //if(!is_valid_object(heap[ptr]))
  //  assert(false);

  assert(heap[last_segment + 1] == null);

  //make the current last segment's 'next' (so to speak) 
  //field point to one address above the deallocated segment 
  heap[last_segment + 1] = ptr - 1;

  heap[ptr] = null;

  //set the length field of the newly added segment
  //(is this required, since the length was already set?)
  //heap[heap[last_segment]] = heap[ptr - 1];

  last_segment = ptr - 1;

}

void gc()
{
#ifdef CUSTOM_BST
  destroy(&black);
  destroy(&grey);
#else
  destroy(black);
  destroy(grey);
#endif

  //only top_level_env is stored in the grey set initially
  build_grey_set();

  while(!is_set_empty(grey))
  {
    //we can pick any grey object,
    //picking the root for convenience
#ifdef CUSTOM_BST
    OBJECT_PTR obj = grey->key;
#else
    OBJECT_PTR obj = *(unsigned int *)grey->root->left->key;
#endif

    insert_node(BLACK, obj);

#ifdef CUSTOM_BST
    remove_node(&grey, obj);
#else
    remove_node(grey, obj);
#endif

    if(IS_CONS_OBJECT(obj))
    {
      OBJECT_PTR car_obj = car(obj);
      if(is_dynamic_memory_object(car_obj))
      {
	insert_node(GREY, car_obj);
#ifdef CUSTOM_BST
	remove_node(&white, car_obj);
#else
	remove_node(white, car_obj);
#endif
      }

      OBJECT_PTR cdr_obj = cdr(obj);
      if(is_dynamic_memory_object(cdr_obj))
      {
	insert_node(GREY, cdr_obj);
#ifdef CUSTOM_BST
	remove_node(&white, cdr_obj);
#else
        remove_node(white, cdr_obj);
#endif
      }
    }
    else if(IS_CLOSURE_OBJECT(obj) || IS_MACRO_OBJECT(obj))
    {
      OBJECT_PTR env_obj = get_env_list(obj);
      if(is_dynamic_memory_object(env_obj))
      {
	insert_node(GREY, env_obj);
#ifdef CUSTOM_BST
	remove_node(&white, env_obj);
#else
	remove_node(white, env_obj);
#endif
      }

      OBJECT_PTR params_obj = get_params_object(obj);
      if(is_dynamic_memory_object(params_obj))
      {
	insert_node(GREY, params_obj);
#ifdef CUSTOM_BST
	remove_node(&white, params_obj);
#else
	remove_node(white, params_obj);
#endif
      }

      OBJECT_PTR body_obj = get_body_object(obj);
      if(is_dynamic_memory_object(body_obj))
      {
	insert_node(GREY, body_obj);
#ifdef CUSTOM_BST
	remove_node(&white, body_obj);
#else
	remove_node(white, body_obj);
#endif
      }
    }
    else if(IS_ARRAY_OBJECT(obj))
    {
      RAW_PTR ptr = obj >> OBJECT_SHIFT;

      OBJECT_PTR length_obj = get_heap(ptr);

      insert_node(GREY, length_obj);
#ifdef CUSTOM_BST
      remove_node(&white, length_obj);
#else
      remove_node(white, length_obj);
#endif

      int len = get_int_value(get_heap(ptr));

      int i;

      for(i=1; i<=len; i++)
      {
        OBJECT_PTR array_elem = get_heap(ptr + i);

        if(is_dynamic_memory_object(array_elem))
        {
          insert_node(GREY, array_elem);
#ifdef CUSTOM_BST
          remove_node(&white, array_elem);
#else
          remove_node(white, array_elem);
#endif
        }
      }
    }
    else if(IS_CONTINUATION_OBJECT(obj))
    {
      OBJECT_PTR stack = ((obj >> OBJECT_SHIFT) << OBJECT_SHIFT) + CONS_TAG;
      insert_node(GREY, stack);
#ifdef CUSTOM_BST
      remove_node(&white, stack);
#else
      remove_node(white, stack);
#endif
    }

    //though integer and float objects are also allocated 
    //on the heap, they don't reference other objects
    //and hence just freeing them is sufficient

  } //end of while(!is_set_empty(grey))

  //free all the objects in the white set
  while(!is_set_empty(white))
  {
#ifdef CUSTOM_BST
    struct node *white_obj = white;
#else
    rb_red_blk_node *white_obj = white->root->left;
#endif

#ifdef CUSTOM_BST
    if(!value_exists(black, white_obj->key))
      dealloc(white_obj->key >> OBJECT_SHIFT);

    remove_node(&white, white_obj->key);
#else
    if(!value_exists(black, *(unsigned int *)white_obj->key))
      dealloc(*(unsigned int *)white_obj->key >> OBJECT_SHIFT);

    remove_node(white,*(unsigned int *)white_obj->key);
#endif
  }

#ifdef CUSTOM_BST
  destroy(&black);
  destroy(&grey);
  destroy(&white);
#else
  destroy(black);
  destroy(grey);
  destroy(white);
#endif

}

BOOLEAN is_dynamic_memory_object(OBJECT_PTR obj)
{
   return IS_CONS_OBJECT(obj)    ||
          IS_ARRAY_OBJECT(obj)   ||
          IS_INTEGER_OBJECT(obj) ||
          IS_FLOAT_OBJECT(obj)   ||
          IS_CLOSURE_OBJECT(obj) ||
          IS_MACRO_OBJECT(obj)   ||
          IS_CONTINUATION_OBJECT(obj);
}

void build_grey_set()
{
  if(top_level_env != NIL)
  {
    insert_node(GREY, top_level_env);
#ifdef CUSTOM_BST
    remove_node(&white, top_level_env);
#else
    remove_node(white,top_level_env);
#endif
  }
}

int get_free_memory()
{
  int free_mem = 0;

  RAW_PTR it = free_list;

  while(it != null)
  {
    free_mem += heap[it];
    it = heap[it + 1];
  }

  return free_mem;
}

void test_memory()
{
  printf("Testing memory\n");

  OBJECT_PTR ptr;

  printf("Allocating 50000 objects...");
  ptr = object_alloc(50000, CONS_TAG);
  printf("done\n");

  printf("Deallocating 50000 objects...");
  dealloc(ptr);
  printf("done\n");

  printf("Allocating 50000 objects...");
  ptr = object_alloc(50000, CONS_TAG);
  printf("done\n");

  printf("Deallocating 50000 objects...");
  dealloc(ptr);
  printf("done\n");  
}

#ifdef CUSTOM_BST

struct node *create_node(OBJECT_PTR value)
{
  struct node *n = (struct node *)malloc(sizeof(struct node));
  n->left = NULL;
  n->right = NULL;
  n->key = value;

  return n;
}

void insert_node(unsigned int set_type, OBJECT_PTR val)
{

#ifdef DEBUG
  assert(set_type == WHITE || set_type == GREY || set_type == BLACK);
#endif

  if(set_type == WHITE)
    white = put(white, val);
  else if(set_type == GREY)
    grey = put(grey, val);
  else
    black = put(black, val);
}

struct node *put(struct node *x, OBJECT_PTR val)
{
  if(x == NULL)
    return create_node(val);

  if(val > x->key)
    x->right = put(x->right, val);
  else if(val < x->key)
    x->left = put(x->left, val);
  else
    x->key = val;

  return x;
}

void insert_node_orig(struct node **r, struct node *n)
{
  struct node *root;

#ifdef DEBUG
  if(!is_valid_object(n->key))
    assert(false);

  if(!is_dynamic_memory_object(n->key))
    assert(false);
#endif

  if(*r == NULL)
    *r = n;
  else
  {
    root = *r;
    if(n->key > root->key)
      insert_node_orig(&(root->right), n);
    else if(n->key < root->key)
      insert_node_orig(&(root->left), n);
  }
}

void remove_node(struct node **r, OBJECT_PTR value)
{
  struct node *root;

  if(!is_valid_object(value))
    assert(false);

  if(*r == NULL)
    return;

  root = *r;

  OBJECT_PTR key = root->key;

  if(value > key)
    remove_node(&(root->right), value);
  else if(value < key)
    remove_node(&(root->left), value);
  else
  {
    if(root->right == NULL && root->left == NULL) //both children are empty
    {
      free(root);
      *r = NULL;
    }
    else if(root->right == NULL) //node has only one child (left)
    {
      root->key = root->left->key;
      *r = root->left;
      free(root);
    }
    else if(root->left == NULL) //node has only one child (right)
    {
      root->key = root->right->key;
      *r = root->right;
      free(root);
    }
    else // both children are present
    {
      //to prevent the tree from becoming unbalanced
      //if(rand() > RAND_MAX / 2)
      if(1) //TODO: fix this
      {
	//in-order successor is the left-most leaf in the 
	//right sub-tree of the node
	struct node *it = root->right;
	struct node *parent = root;

	while(it->left != NULL)
	{
	  parent = it;
	  it = it->left;
	}

	root->key = it->key;
	
	if(parent == root)
	  parent->right = it->right;
	else
	  parent->left = NULL;

	free(it);

      }
      else
      {
	//in-order predecessor is the right-most leaf in the
	//left sub-tree of the node
	struct node *it = root->left;
	struct node *parent = root;

	while(it->right != NULL)
	{
	  parent = it;
	  it = it->right;
	}

	root->key = it->key;
	
	if(parent == root)
	  parent->left = it->left;
	else
	  parent->right = NULL;

	free(it);
      
      }
    }
  }
}

void destroy(struct node **r)
{
  struct node *root = *r;

  if(root == NULL)
    return;

  destroy(&(root->left));
  destroy(&(root->right));
  
  free(root);
  *r = NULL;
}

void print_tree(struct node *n)
{
  if(n == NULL)
    return;

  if(n->left != NULL)
    print_tree(n->left);

  //fprintf(stdout, "%d\n", n->key);
  print_object(n->key); printf("\n");

  if(n->right != NULL)
    print_tree(n->right);

}

BOOLEAN is_set_empty(struct node *set)
{
  if(set == NULL)
    return true;
  else
    return false;
}

int get_size_of_tree(struct node *n)
{
  if(n == NULL)
    return 0;
  else
    return 1 + get_size_of_tree(n->left) + get_size_of_tree(n->right);
}

int get_height_of_tree(struct node *n)
{
  if(n == NULL)
    return -1;
  else
  {
    int l = get_height_of_tree(n->left);
    int r = get_height_of_tree(n->right);
    return 1 + ((l>r) ? l : r);
  }
}

void test_bst()
{

  printf("BST test start\n");

  grey = NULL;

  //test 1

  /* int i; */
  /* int a[1000]; */

  /* for(i=0; i< 1000; i++) */
  /* { */
  /*   a[i] = rand(); */
  /*   insert_node(GREY, a[i]); */
  /* } */

  /* print_tree(grey); */

  /* printf("removing nodes\n"); */

  /* for(i=999; i>= 0; i--) */
  /*   remove_node(&grey, a[i]); */

  /* print_tree(grey); */

  /* if(is_set_empty(grey)) */
  /*   printf("set is empty\n"); */

  //end of test 1

  //test 2

  /* insert_node(GREY, 10); */
  /* insert_node(GREY, 5); */
  /* insert_node(GREY, 20); */
  /* insert_node(GREY, 3); */
  /* insert_node(GREY, 8); */
  /* insert_node(GREY, 15); */
  /* insert_node(GREY, 40); */
  /* insert_node(GREY, 13); */
  /* insert_node(GREY, 18); */

  /* print_tree(grey); */

  /* remove_node(&grey, 15); */

  /* if(value_exists(grey, 10)) */
  /*    printf("10 exists in the tree\n"); */
  /* else */
  /*   printf("Failure: 10 does not exist in the tree\n"); */

  /* if(!value_exists(grey, 15)) */
  /*    printf("15 does not exist in the tree\n"); */
  /* else */
  /*   printf("Failure: 15 exists not exist in the tree\n"); */

  /* print_tree(grey); */

  //end of test 2

  //test 3
  insert_node(GREY, 100);
  insert_node(GREY, 80);
  insert_node(GREY, 60);
  insert_node(GREY, 40);
  insert_node(GREY, 120);
  insert_node(GREY, 110);
  insert_node(GREY, 130);
  insert_node(GREY, 90);
  insert_node(GREY, 95);

  print_tree(grey);
  printf("----%d------\n", get_size_of_tree(grey));

  remove_node(&grey, 120);

  print_tree(grey);
  printf("----%d------\n", get_size_of_tree(grey));

  //end of test 3

  destroy(&grey);

  printf("BST test done\n");
}

BOOLEAN value_exists(struct node *r, RAW_PTR val)
{
  if(r == NULL)
    return false;

  if(r->key == val)
    return true;
  else if(r->key > val)
    return value_exists(r->left, val);
  else
    return value_exists(r->right, val);

  //return value_exists(r->left, val) || value_exists(r->right, val);
}

#endif

void IntDest(void* a)
{
  free((unsigned int*)a);
}

int IntComp(const void* a,const void* b)
{
  if( *(unsigned int*)a > *(unsigned int*)b) return(1);
  if( *(unsigned int*)a < *(unsigned int*)b) return(-1);
  return(0);
}

void IntPrint(const void* a)
{
  printf("%i",*(unsigned int*)a);
}

void InfoPrint(void* a)
{
  ;
}

void InfoDest(void *a)
{
  ;
}

void insert_node(unsigned int set_type, OBJECT_PTR val)
{

#ifdef DEBUG
  assert(set_type == WHITE || set_type == GREY || set_type == BLACK);
#endif

  rb_red_blk_tree *tree;

  if(set_type == WHITE)
    tree = white;
  else if(set_type == GREY)
    tree = grey;
  else
    tree = black;

  if(tree == NULL)
    tree = RBTreeCreate(IntComp,IntDest,InfoDest,IntPrint,InfoPrint);

  unsigned int *newInt=(unsigned int*) malloc(sizeof(unsigned int));
  *newInt = val;
  RBTreeInsert(tree,newInt,0);  
}

void remove_node(rb_red_blk_tree *tree, OBJECT_PTR val)
{
  if(!tree)
    return;

  rb_red_blk_node* newNode;
  if((newNode=RBExactQuery(tree,&val))) 
    RBDelete(tree,newNode);  
}

void destroy(rb_red_blk_tree *tree)
{
  if(tree)
    RBTreeDestroy(tree);
  tree = NULL;
}

void print_tree(rb_red_blk_tree *tree)
{
  RBTreePrint(tree);
}

BOOLEAN is_set_empty(rb_red_blk_tree *tree)
{
  if(!tree)
    return true;

  return tree->root->left->parent == NULL;
}

BOOLEAN value_exists(rb_red_blk_tree *tree, OBJECT_PTR val)
{
  rb_red_blk_node* newNode;
  if((newNode=RBExactQuery(tree,&val)))
    return true;
  else
    return false;
}
