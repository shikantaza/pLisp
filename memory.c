#include <stdio.h>
#include <stdlib.h>

#include "plisp.h"

RAW_PTR *heap;
RAW_PTR free_list;

//we don't need the black set, actually
struct node *black = NULL, *white = NULL, *grey = NULL;

extern OBJECT_PTR init_env_list, NIL;

void initialize_free_list()
{
  free_list = 0;
  heap[free_list] = HEAP_SIZE;
  heap[free_list + 1] = null;

  //test code

  //dealloc(object_alloc(100));
  //dealloc(object_alloc(20));

  /*
  black = NULL;

  int i;
  int a[100];

  for(i=0; i< 100; i++)
  {
    a[i] = rand();
    insert_node(&black, create_node(a[i]));
  }

  print_tree(black);

  printf("removing nodes\n");

  for(i=99; i>= 0; i--)
    remove_node(&black, a[i]);

  print_tree(black);

  if(is_set_empty(black))
    printf("set is empty\n");

  destroy(black);
  */
  //end test code

}

//first-fit (use the first segment
//whose length is closest
//to the required size)
RAW_PTR object_alloc(int size)
{
  RAW_PTR it = free_list;
  RAW_PTR prev = null;

  RAW_PTR ret;

  while(it != null)
  {
    if(heap[it] >= (size + 4))
    {
      //the current segment will not have any
      //usable space after the allocation, 
      //so it will have to be
      //removed from the free list
      //[we allocate the entire segment
      //though the caller will not (and should not)
      //use the additional memory]
      if(heap[it] == (size + 4))
      {
	//point the previous segment's 'next' to
	//the current segment's 'next
	//(i.e., remove the current segment
	//from the free list)
	heap[prev] = heap[it + 1]; 

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

	//insert the segment into the 'white' set.
	//all objects are initially placed in the white set;
	//during GC objects that should not freed are
	//moved to the grey and, subsequently, the black sets

	//this is now done after the call to object_alloc in the calling function
	//insert_node(&white, create_node(ret << FN_SHIFT));

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

RAW_PTR get_last_segment()
{
  RAW_PTR it = free_list;

  while(heap[it + 1] != null)
    it = heap[it + 1];

  return it;

}

void dealloc(RAW_PTR ptr)
{
  RAW_PTR last_segment = get_last_segment();

  //make the current last segment's 'next' (so to speak) 
  //field point to one address above the deallocated segment 
  heap[last_segment + 1] = ptr - 1;

  heap[ptr] = null;

  //set the length field of the newly added segment
  //(is this required, since the length was already set?)
  heap[heap[last_segment]] = heap[ptr - 1];

  //heap[heap[last_segment] + 1] = null;
}

struct node *create_node(OBJECT_PTR value)
{
  struct node *n = (struct node *)malloc(sizeof(struct node));
  n->left = NULL;
  n->right = NULL;
  n->key = value;

  return n;
}

void insert_node(struct node **r, struct node *n)
{
  struct node *root;

  if(*r == NULL)
    *r = n;
  else
  {
    root = *r;
    if(n->key > root->key)
      insert_node(&(root->right), n);
    else if(n->key < root->key)
      insert_node(&(root->left), n);
  }
}

void remove_node(struct node **r, OBJECT_PTR value)
{
  struct node *root;

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
	  parent->right = NULL;
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
	  parent->left = NULL;
	else
	  parent->right = NULL;

	free(it);
      
      }
    }
  }
}

void destroy(struct node *root)
{
  if(root == NULL)
    return;

  destroy(root->left);
  destroy(root->right);
  
  free(root);
  root = NULL;
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

void gc()
{
  //int f1 = get_free_memory();

  /* fprintf(stdout, "\nAvailable memory before GC = %d\n", get_free_memory()); */

  //destroy(black);
  destroy(grey);

  struct node *grey_obj;

  //all root objects (i.e., those in init_env_list)
  //are stored in the grey set initially
  build_grey_set();

  /* printf("Before starting GC:\n"); */
  /* printf("WHITE:\n"); */
  /* print_tree(white); */
  /* printf("\n"); */
  /* getchar(); */

  while(!is_set_empty(grey))
  {

    /* printf("Before GC iteration:\n"); */
    /* printf("GREY:\n"); */
    /* print_tree(grey); */
    /* printf("\n"); */
    /* getchar(); */

    //we can pick any grey object,
    //picking the root for convenience
    grey_obj = grey;

    OBJECT_PTR obj = grey_obj->key;

    //insert_node(&black, create_node(obj));
    remove_node(&grey, obj);

    if(IS_CONS_OBJECT(obj))
    {
      OBJECT_PTR car_obj = car(obj);
      if(is_dynamic_memory_object(car_obj))
      {
	insert_node(&grey, create_node(car_obj));
	remove_node(&white, car_obj);
      }

      OBJECT_PTR cdr_obj = cdr(obj);
      if(is_dynamic_memory_object(cdr_obj))
      {
	insert_node(&grey, create_node(cdr_obj));
	remove_node(&white, cdr_obj);
      }
    }
    else if(IS_FN_OBJECT(obj) || IS_MACRO_OBJECT(obj))
    {
      OBJECT_PTR env_obj = get_env_list(obj);
      if(is_dynamic_memory_object(env_obj))
      {
	insert_node(&grey, create_node(env_obj));
	remove_node(&white, env_obj);
      }

      OBJECT_PTR params_obj = get_params_object(obj);
      if(is_dynamic_memory_object(params_obj))
      {
	insert_node(&grey, create_node(params_obj));
	remove_node(&white, params_obj);
      }

      OBJECT_PTR body_obj = get_body_object(obj);
      if(is_dynamic_memory_object(body_obj))
      {
	insert_node(&grey, create_node(body_obj));
	remove_node(&white, body_obj);
      }
    }
    /*
    else if(IS_MACRO_OBJECT(obj))
    {
      OBJECT_PTR env_obj = car(obj);
      if(is_dynamic_memory_object(env_obj))
      {
	insert_node(&grey, create_node(env_obj));
	remove_node(&white, env_obj);
      }

      OBJECT_PTR params_obj = CADR(obj);
      if(is_dynamic_memory_object(params_obj))
      {
	insert_node(&grey, create_node(params_obj));
	remove_node(&white, params_obj);
      }

      OBJECT_PTR body_obj = CDDR(obj);
      if(is_dynamic_memory_object(body_obj))
      {
	insert_node(&grey, create_node(body_obj));
	remove_node(&white, body_obj);
      }
    }
    */
    else if(IS_ARRAY_OBJECT(obj))
    {
      RAW_PTR ptr = obj >> ARRAY_SHIFT;

      int len = get_int_value(heap[ptr]);

      int i;

      for(i=1; i<=len; i++)
      {
	OBJECT_PTR array_elem = heap[ptr + i];
	if(is_dynamic_memory_object(array_elem))
	{
	  insert_node(&grey, create_node(array_elem));
	  remove_node(&white, array_elem);
	}
      }
    }
    //though float objects are also allocated 
    //on the heap, they don't reference other objects
    //and hence just freeing them is sufficient

  } //end of while(!is_set_empty(grey))

  /* printf("After GC:\n"); */

  /* printf("BLACK:\n"); */
  /* print_tree(black); */
  /* printf("\n"); */
  /* getchar(); */

  /* printf("WHITE (to be recycled):\n"); */
  /* print_tree(white); */
  /* printf("\n"); */
  /* getchar(); */

  struct node *white_obj;

  //free all the objects in the white set
  while(!is_set_empty(white))
  {
    white_obj = white;
    dealloc(white_obj->key >> CONS_SHIFT);
    remove_node(&white, white_obj->key);
  }

  //destroy(black);
  destroy(grey);
  destroy(white);

  /* fprintf(stdout, "Available memory after GC = %d\n", get_free_memory()); */
  //fprintf(stdout, "\n%d bytes of memory freed\n", 4 * (get_free_memory() - f1));

}

BOOLEAN is_set_empty(struct node *set)
{
  if(set == NULL)
    return true;
  else
    return false;
}

BOOLEAN is_dynamic_memory_object(OBJECT_PTR obj)
{
  return IS_CONS_OBJECT(obj)  ||
         IS_ARRAY_OBJECT(obj) ||
         IS_FLOAT_OBJECT(obj) ||
         IS_FN_OBJECT(obj)    ||
         IS_MACRO_OBJECT(obj);
}

void build_grey_set()
{

  insert_node(&grey, create_node(init_env_list));
  remove_node(&white, init_env_list);

  /*
  OBJECT_PTR rest = car(init_env_list);

  while(rest != NIL)
  {
    OBJECT_PTR obj = CDAR(rest);

    if(is_dynamic_memory_object(obj))
    {
      insert_node(&grey, create_node(obj));
      remove_node(&white, obj);
    }

    rest = cdr(rest);
  }
  */
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

int get_size_of_tree(struct node *n)
{
  if(n == NULL)
    return 0;
  else
    return 1 + get_size_of_tree(n->left) + get_size_of_tree(n->right);
}
