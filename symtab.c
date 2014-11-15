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
#include "globals.h"

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
void st_insert_( ScopeList scope, char * name, ExpType type, int lineno, int loc )
{ int h = hash(name);
  BucketList l = scope->bucket[h];
  while ((l != NULL) && (strcmp(name,l->name) != 0))
    l = l->next;
  if (l == NULL) /* variable not yet in table */
  { l = (BucketList) malloc(sizeof(struct BucketListRec));
    l->name = name;
    l->lines = (LineList) malloc(sizeof(struct LineListRec));
    l->type = type;
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

void st_insert( char * scope, char * name, ExpType type, int lineno, int loc )
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
  st_insert_(l, name, type, lineno, loc);
}/* st_insert */

/* Function st_lookup returns the memory 
 * location of a variable or -1 if not found
 */
BucketList st_lookup_ ( ScopeList scope, char * name )
{ int h = hash(name);
  if (scope == NULL)
    return NULL;
  BucketList l = scope->bucket[h];
  while ((l != NULL) && (strcmp(name,l->name) != 0))
    l = l->next;
  if (l == NULL) return NULL;
  else return l;
}

BucketList st_lookup ( char * scope, char * name )
{ BucketList result;
  int h = hash(scope);
  ScopeList l = scopeHashTable[h];
  while ((l != NULL) && (strcmp(scope,l->name) != 0))
    l = l->next;
  if (l == NULL)
  { ScopeList parent = getParentScope(scope);
    if (parent == NULL)
      return NULL;
    return st_lookup(parent->name, name);
  }

  result = st_lookup_(l, name);
  if (result == NULL) //lookup in parent
  { ScopeList parent = getParentScope(scope);
    if (parent == NULL) //if there is no parent
      return NULL;
    return st_lookup(parent->name, name);
  }
  else return result;
}

BucketList st_lookup_excluding_parent ( char * scope, char * name )
{ int result;
  int h = hash(scope);
  ScopeList l = scopeHashTable[h];
  while ((l != NULL) && (strcmp(scope,l->name) != 0))
    l = l->next;

  if (l == NULL)
      return NULL;
  
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
  fprintf(listing,"Variable Name  Variable Type  Location   Line Numbers\n");
  fprintf(listing,"-------------  -------------  --------   ------------\n");
  for (i=0;i<SIZE;++i)
  { if (scope->bucket[i] != NULL)
    { BucketList l = scope->bucket[i];
      while (l != NULL)
      { LineList t = l->lines;
        fprintf(listing,"%-14s ",l->name);
        fprintf(listing,"%-13s ",l->type == Integer? "Integer" : "Void");
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
