/****************************************************/
/* File: symtab.c                                   */
/* Symbol table implementation for the TINY compiler*/
/* (allows only one symbol table)                   */
/* Symbol table is implemented as a chained         */
/* hash table                                       */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"

/* SIZE is the size of the hash table */
#define SIZE 211

/* SHIFT is the power of two used as multiplier
   in hash function  */
#define SHIFT 4

/* the hash function */
static int hash ( char * key )
{ int temp = 0;
  int i = 0;
  while (key[i] != '\0')
  { temp = ((temp << SHIFT) + key[i]) % SIZE;
    ++i;
  }
  return temp;
}

/* the list of line numbers of the source 
 * code in which a variable is referenced
 */
typedef struct LineListRec
   { int lineno;
     struct LineListRec * next;
   } * LineList;

/* The record in the bucket lists for
 * each variable, including name, 
 * assigned memory location, and
 * the list of line numbers in which
 * it appears in the source code
 */
typedef struct BucketListRec
   { char * name;
     LineList lines;
     int memloc ; /* memory location for variable */
     struct BucketListRec * next;
   } * BucketList;

/* The record for each scope,
 * including name, its bucket,
 * and parent scpoe.
 */
typedef struct ScopeListRec
   { char * name;
     BucketList bucket[SIZE];
     struct ScopeListRec * parent;
     struct ScopeListRec * next;
   } * ScopeList;

/* the scope table */
static ScopeList scopeHashTable[SIZE];

ScopeList getParentScope(char * scope)
{ int i;
  char *tmp;
  ScopeList l;
  if (strcmp(scope, "") == 0)
    return NULL;
  tmp = (char *)malloc(sizeof(char) *(strlen(scope) + 1));
  strcpy(tmp, scope);
  for(i = strlen(tmp); 0 <= i; i--)
  { if (tmp[i] == ':')
    { tmp[i] = '\0';
      break; }
    else
      tmp[i] = '\0';
  }
  l = scopeHashTable[hash(tmp)];
  while ((l != NULL) && (strcmp(tmp,l->name) != 0))
    l = l->next;
  
  if (l == NULL)
    l = getParentScope(tmp);
  free(tmp);
  return l;
}

/* Procedure st_insert_ inserts line numbers and
 * memory locations into the symbol table
 * loc = memory location is inserted only the
 * first time, otherwise ignored
 */
void st_insert_( ScopeList scope, char * name, int lineno, int loc )
{ int h = hash(name);
  BucketList l = scope->bucket[h];
  while ((l != NULL) && (strcmp(name,l->name) != 0))
    l = l->next;
  if (l == NULL) /* variable not yet in table */
  { l = (BucketList) malloc(sizeof(struct BucketListRec));
    l->name = name;
    l->lines = (LineList) malloc(sizeof(struct LineListRec));
    l->lines->lineno = lineno;
    l->memloc = loc;
    l->lines->next = NULL;
    l->next = scope->bucket[h];
    scope->bucket[h] = l; }
  else /* found in table, so just add line number */
  { LineList t = l->lines;
    while (t->next != NULL) t = t->next;
    t->next = (LineList) malloc(sizeof(struct LineListRec));
    t->next->lineno = lineno;
    t->next->next = NULL;
  }
}

void st_insert( char * scope, char * name, int lineno, int loc )
{ int h = hash(scope);
  ScopeList l = scopeHashTable[h];
  while ((l != NULL) && (strcmp(scope,l->name) != 0))
    l = l->next;
  if (l == NULL) /* scope not yet in table */
  { l = (ScopeList) malloc(sizeof(struct ScopeListRec));
    l->parent = getParentScope(scope);
    l->name = (char *)malloc(sizeof(char) * (strlen(scope) + 1));
    strcpy(l->name, scope);
    l->next = scopeHashTable[h];
    scopeHashTable[h] = l; }
  st_insert_(l, name, lineno, loc);
}/* st_insert */

/* Function st_lookup returns the memory 
 * location of a variable or -1 if not found
 */
int st_lookup_ ( ScopeList scope, char * name )
{ int h = hash(name);
  if (scope == NULL)
    return -1;
  BucketList l = scope->bucket[h];
  while ((l != NULL) && (strcmp(name,l->name) != 0))
    l = l->next;
  if (l == NULL) return -1;
  else return l->memloc;
}

int st_lookup ( char * scope, char * name )
{ int result;
  int h = hash(scope);
  ScopeList l = scopeHashTable[h];
  while ((l != NULL) && (strcmp(scope,l->name) != 0))
    l = l->next;
  if (l == NULL)
  { ScopeList parent = getParentScope(scope);
    if (parent == NULL)
      return -1;
    return st_lookup(parent->name, name);
  }

  result = st_lookup_(l, name);
  if (result == -1) //lookup in parent
  { ScopeList parent = l->parent;
    if (parent == NULL) //if there is no parent
      return -1;
    return st_lookup(parent->name, name);
  }
  else return result;
}

int st_lookup_excluding_parent ( char * scope, char * name )
{ int result;
  int h = hash(scope);
  ScopeList l = scopeHashTable[h];
  while ((l != NULL) && (strcmp(scope,l->name) != 0))
    l = l->next;

  if (l == NULL)
      return -1;
  
  return st_lookup_(l, name);
}

void addline( char * scope, char * name, int lineno )
{ BucketList bucket;
  LineList lineList;
  ScopeList l = scopeHashTable[hash(scope)];
  while ((l != NULL) && (strcmp(scope, l->name)))
    l = l->next;
  if (l == NULL)
    l = getParentScope(scope);
  while (l)
  { bucket = l->bucket[hash(name)];
    while ((bucket != NULL) && (strcmp(name, bucket->name) != 0))
      bucket = bucket->next;
    if (bucket == NULL)
      l = l->parent;
    else
      break;
  }
  lineList = bucket->lines;
  while(lineList->next != NULL) lineList = lineList->next;
  lineList->next = (LineList) malloc(sizeof(struct LineListRec));
  lineList->next->lineno = lineno;
  lineList->next->next = NULL;
}

/* Procedure printSymTab prints a formatted 
 * listing of the symbol table contents 
 * to the listing file
 */
void printSymTab_(FILE * listing, ScopeList scope)
{ int i;
  fprintf(listing,"Variable Name  Location   Line Numbers\n");
  fprintf(listing,"-------------  --------   ------------\n");
  for (i=0;i<SIZE;++i)
  { if (scope->bucket[i] != NULL)
    { BucketList l = scope->bucket[i];
      while (l != NULL)
      { LineList t = l->lines;
        fprintf(listing,"%-14s ",l->name);
        fprintf(listing,"%-8d  ",l->memloc);
        while (t != NULL)
        { fprintf(listing,"%4d ",t->lineno);
          t = t->next;
        }
        fprintf(listing,"\n");
        l = l->next;
      }
    }
  }
} 

void printSymTab(FILE * listing)
{ int i;
  for (i=0;i<SIZE;++i)
  { if (scopeHashTable[i] != NULL)
    { ScopeList l = scopeHashTable[i];
      while ( l != NULL)
      { fprintf(listing,"Scope name : %s\n", l->name);
        fprintf(listing,"----------------------------------\n");
        printSymTab_(listing, l);
        fprintf(listing,"----------------------------------\n\n\n\n");
        l = l->next;
      }
    }
  }
} /* printSymTab */
