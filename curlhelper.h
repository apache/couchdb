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

#ifndef CURLHELPER_H
#define CURLHELPER_H

typedef struct {
  char* data;
  int count;
  int capacity;
}* Buffer;

Buffer init_Buffer();
void free_Buffer(Buffer b);
int append_Buffer(Buffer b,char* c,int length);
// WARNING USES MALLOC DONT FORGET TO FREE
char* toString_Buffer(Buffer b);
int print_Buffer(Buffer b);
int shrink_Buffer(Buffer b);
void copy_Buffer(Buffer b, char* c, int pos, int length);


typedef struct {
  void** elements;
  int count;
  int capacity;
}* List;

List init_List(int capacity);
void free_List(List l);
void* get_List(List l, int pos);
void* pull_List(List l);
int set_List(List l, int pos, void* ptr);
int append_List(List l, void* ptr, int length);
int push_List(List l, void* ptr, int length);

#endif
