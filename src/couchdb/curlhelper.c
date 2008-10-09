/*

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License.  You may obtain a copy of the
License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the License for the
specific language governing permissions and limitations under the License.

*/

#include <stdlib.h>
#include <stdio.h>
#include "curlhelper.h"

#define TRUE 1
#define FALSE 0

Buffer init_Buffer() {
  Buffer b;

  if((b = (Buffer)malloc(sizeof(char*) + sizeof(int)*2)) == NULL) {
    return NULL;
  }

  b->count = 0;
  b->capacity = 50;

  if(b->data = (char*)malloc(sizeof(char)*b->capacity)) {
    return b;
  } else {
    return NULL;
  }
}

void free_Buffer(Buffer b) {
  if(b == NULL) 
    return;
  if(b->data != NULL)
    free(b->data);
  free(b);
}

int append_Buffer(Buffer b, char* c, int length) {
  int capacity_changed;
  int new_count;

  capacity_changed = FALSE;
  new_count = b->count + length;

  if(new_count > b->capacity) {
    capacity_changed = TRUE;
    b->capacity *= 2;
  }

  while(new_count > b->capacity) {
    b->capacity *= 2;
  }

  if(capacity_changed) {
    if((b->data = (char*)realloc(b->data,b->capacity)) == NULL) {
      return FALSE;
    }
  }

  int i;

  for(i = 0;i < length;i++) {
    *(b->data + b->count + i) = *(c + i);
  }

  b->count = new_count;

  return TRUE;
}

char* toString_Buffer(Buffer b) {
  char* result;

  if((result = (char*)malloc(sizeof(char)*(b->count+1))) == NULL) {
    return NULL;
  }

  result[b->count] = '\0';

  int i;

  for(i = 0;i < b->count;i++) {
    result[i] = b->data[i];
  }

  return result;
}

int print_Buffer(Buffer b) {
  char* c;

  if((c = toString_Buffer(b)) == NULL) {
    return FALSE;
  }

  printf("%s\n",c);

  free(c);

  return TRUE;
}


int shrink_Buffer(Buffer b) {
  b->capacity = b->count;

  if((b->data = realloc(b->data,sizeof(char)*b->capacity)) == NULL) {
    return FALSE;
  } else {
    return TRUE;
  }
}

void copy_Buffer(Buffer b, char* c, int pos, int length) {
  if((pos + length) > b->count)
    return;

  int i;

  for(i = 0; i < length;i++) {
    *(c + i) = *(b->data + pos + i);
  }
}


List init_List(int capacity) {
  if(capacity < 5)
    capacity = 5;

  List l;

  if((l = (List)malloc(sizeof(void**)+sizeof(int)*2)) == NULL) {
    return NULL;
  }

  l->count = 0;
  l->capacity = capacity;

  if((l->elements = (void**)malloc(sizeof(void*)*l->capacity)) == NULL) {
    return NULL;
  }

  return l;
}

void free_List(List l) {
  if(l == NULL)
    return;
  if(l->elements != NULL)
    free(l->elements);
  free(l);
}

void* get_List(List l, int pos) {
  if(pos > (l->count - 1)) {
    return NULL;
  }
  return *(l->elements + pos);
}

void* pull_List(List l) {
  void* r = *(l->elements);

  int i;

  for(i = 1; i < (l->count-1);i++) {
    l->elements[i] = l->elements[i+1];
  }
  l->count -= 1;
  return r;
}

int set_List(List l, int pos, void* ptr) {
  if(pos > (l->count - 1)) {
    return FALSE;
  }

  *(l->elements + pos) = ptr;
   
  return TRUE;
}

int append_List(List l, void* ptr, int length) {
  int capacity_changed;
  int new_count;

  capacity_changed = FALSE;
  new_count = l->count + length;

  if(new_count > l->capacity) {
    capacity_changed = TRUE;
    l->capacity *= 2;
  }

  while(new_count > l->capacity) {
    l->capacity *= 2;
  }

  if(capacity_changed) {
    if((l->elements = (void*)realloc(l->elements,l->capacity)) == NULL) {
      return FALSE;
    }
  }

  int i;

  for(i = 0;i < length;i++) {
    *(l->elements + l->count + i) = ptr + i;
  }

  l->count = new_count;

  return TRUE;
}

int push_List(List l, void* ptr, int length) {
  int capacity_changed;
  int new_count;

  capacity_changed = FALSE;
  new_count = l->count + length;

  if(new_count > l->capacity) {
    capacity_changed = TRUE;
    l->capacity *= 2;
  }

  while(new_count > l->capacity) {
    l->capacity *= 2;
  }

  if(capacity_changed) {
    if((l->elements = (void*)realloc(l->elements,l->capacity)) == NULL) {
      return FALSE;
    }
  }

  int i;

  for(i = 0;i < length;i++) {
    *(l->elements + l->count + i) = *(l->elements + i);
  }

  for(i = 0;i < length;i++) {
    *(l->elements + i) = ptr+i;
  }

  l->count = new_count;

  return TRUE;
}
