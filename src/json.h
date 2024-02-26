/**
  Copyright 2011-2024 Rajesh Jayaprakash <rajesh.jayaprakash@pm.me>

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

enum {JSON_STRING, JSON_INT, JSON_FLOAT, JSON_NAME_VALUE_PAIRS, JSON_ARRAY};

struct JSONObject;

struct name_value_pair
{
  char *name;
  struct JSONObject *value;
};

struct name_value_pairs
{
  int count;
  struct name_value_pair **elements;
};

struct JSONArray
{
  int count;
  struct JSONObject **elements;
};

struct JSONObject
{
  int type;

#if __x86_64__
  long long ivalue;
#else
#ifdef __APPLE__
  long long ivalue;
#else
  int ivalue;
#endif  
#endif
  double fvalue;
  char *strvalue;

  struct name_value_pairs *pairs;

  struct JSONArray *array;
};

struct JSONObject *JSON_create_string_object(char *);
  
#if __x86_64__
struct JSONObject *JSON_create_int_object(long long);
#else
#ifdef __APPLE__
struct JSONObject *JSON_create_int_object(long long);
#else
struct JSONObject *JSON_create_int_object(int);  
#endif
#endif

struct JSONObject *JSON_create_float_object(double);
struct JSONObject *JSON_create_pairs_object(struct name_value_pairs *);
struct name_value_pair *JSON_create_name_value_pair(char *, struct JSONObject *);
struct JSONObject *JSON_create_array_object(struct JSONArray *);
void JSON_print_object(struct JSONObject *);
void JSON_delete_object(struct JSONObject *);
struct JSONObject *JSON_get_object_item(struct JSONObject *, char *);
struct JSONObject *JSON_get_array_item(struct JSONObject *, int);
struct JSONObject *JSON_parse(char *);
int JSON_get_array_size(struct JSONObject *);
