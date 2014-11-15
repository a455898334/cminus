/****************************************************/
/* File: analyze.c                                  */
/* Semantic analyzer implementation                 */
/* for the TINY compiler                            */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "analyze.h"
#include "util.h"

/* counter for variable memory locations */
static int location = 0;
static char * scope = "~";

char * getNewScope(TreeNode * t)
{ char *result = NULL;
  if (t->nodekind == StmtK)
  { if (t->kind.stmt == FunctionK)
    { result = (char *)malloc(sizeof(char) * (strlen(scope) + strlen(t->attr.name) + 3));
      sprintf(result, "%s:%s", scope, t->attr.name);
    }
    else if (t->kind.stmt == CompoundK)
    { result = (char *)malloc(sizeof(char) * (strlen(scope) + 12));
      sprintf(result, "%s:%d\0", scope, t->lineno);
    }
  }
  if (result == NULL)
  { result = (char *)malloc(sizeof(char) * (strlen(scope) + 2));
    strcpy(result, scope);
  }
  return result;
}

/* Procedure traverse is a generic recursive 
 * syntax tree traversal routine:
 * it applies preProc in preorder and postProc 
 * in postorder to tree pointed to by t
 */
static void traverse( TreeNode * t,
               void (* preProc) (TreeNode *),
               void (* postProc) (TreeNode *) )
{ if (t != NULL)
  { 
    preProc(t);
    { int i;
      char * scopeBackup = scope;
      scope = getNewScope(t);

      for (i=0; i < MAXCHILDREN; i++)
        traverse(t->child[i],preProc,postProc);

      free(scope);
      scope = scopeBackup;
    }
    postProc(t);
    traverse(t->sibling,preProc,postProc);
  }
}

/* nullProc is a do-nothing procedure to 
 * generate preorder-only or postorder-only
 * traversals from traverse
 */
static void nullProc(TreeNode * t)
{ if (t==NULL) return;
  else return;
}

/* Procedure insertNode inserts 
 * identifiers stored in t into 
 * the symbol table 
 */
static void insertNode( TreeNode * t)
{ switch (t->nodekind)
  { case StmtK:
      switch (t->kind.stmt)
      { case FunctionK:
          if (st_lookup(scope,t->attr.name) == -1)
          /* not yet in table, so treat as new definition */
            st_insert(scope,t->attr.name,t->type,t->lineno,location++);
          else
          /* already in table, so ignore location, 
             add line number of use only */ 
            //st_insert(t->attr.name,t->lineno,0);
            fprintf(listing, "error:%d: %s is already declared\n", t->lineno, t->attr.name);
          break;
        default:
          break;
      }
      break;
    case ExpK:
      switch (t->kind.exp)
      { case VarK:
        case VarArrayK:
        case SingleParamK:
        case ArrayParamK:
          if (t->attr.name != NULL && st_lookup_excluding_parent(scope,t->attr.name) == -1)
          /* not yet in table, so treat as new definition */
            st_insert(scope,t->attr.name,t->type,t->lineno,location++);
          else if(t->attr.name != NULL)
          /* already in table, so ignore location, 
             add line number of use only */ 
            //st_insert(t->attr.name,t->lineno,0);
            fprintf(listing, "error:%d: %s is already declared\n", t->lineno, t->attr.name);
          break;
        case IdK:
        case CallK:
          if (st_lookup(scope,t->attr.name) == -1)
            fprintf(listing, "error:%d: %s is not declared\n", t->lineno, t->attr.name);
          else
            addline(scope,t->attr.name,t->lineno,0);
          break;
        default:
          break;
      }
      break;
    default:
      break;
  }
}

void insertBuiltinFunctions(TreeNode ** syntaxTree)
{ TreeNode *input = newStmtNode(FunctionK);
  input->sibling = *syntaxTree;
  *syntaxTree = input;
  input->lineno = 0;
  input->attr.name = malloc(sizeof(char) * (strlen("input") + 1));
  strcpy(input->attr.name, "input");
  input->type = Integer;

  TreeNode *output = newStmtNode(FunctionK);
  output->sibling = *syntaxTree;
  *syntaxTree = output;
  output->lineno = 0;
  output->attr.name = malloc(sizeof(char) * (strlen("output") + 1));
  strcpy(output->attr.name, "output");
  output->type = Void;

  TreeNode *param = newExpNode(SingleParamK);
  param->type = Integer;
  param->attr.name = malloc(sizeof(char) * (strlen("arg") + 1));
  strcpy(param->attr.name, "arg");
  param->lineno = 0;

  output->child[0] = param;
}

/* Function buildSymtab constructs the symbol 
 * table by preorder traversal of the syntax tree
 */
void buildSymtab(TreeNode * syntaxTree)
{ insertBuiltinFunctions(&syntaxTree);
  traverse(syntaxTree,insertNode,nullProc);
  if (TraceAnalyze)
  { fprintf(listing,"\nSymbol table:\n\n");
    printSymTab(listing);
  }
}

static void typeError(TreeNode * t, char * message)
{ fprintf(listing,"Type error at line %d: %s\n",t->lineno,message);
  Error = TRUE;
}

/* Procedure checkNode performs
 * type checking at a single tree node
 */
static void checkNode(TreeNode * t)
{ /*switch (t->nodekind)
  { case ExpK:
      switch (t->kind.exp)
      { case OpK:
          if ((t->child[0]->type != Integer) ||
              (t->child[1]->type != Integer))
            typeError(t,"Op applied to non-integer");
          if ((t->attr.op == EQ) || (t->attr.op == LT))
            t->type = Boolean;
          else
            t->type = Integer;
          break;
        case ConstK:
        case IdK:
          t->type = Integer;
          break;
        default:
          break;
      }
      break;
    case StmtK:
      switch (t->kind.stmt)
      { case IfK:
          if (t->child[0]->type == Integer)
            typeError(t->child[0],"if test is not Boolean");
          break;
        case AssignK:
          if (t->child[0]->type != Integer)
            typeError(t->child[0],"assignment of non-integer value");
          break;
        case WriteK:
          if (t->child[0]->type != Integer)
            typeError(t->child[0],"write of non-integer value");
          break;
        case RepeatK:
          if (t->child[1]->type == Integer)
            typeError(t->child[1],"repeat test is not Boolean");
          break;
        default:
          break;
      }
      break;
    default:
      break;

  }*/
}

/* Procedure typeCheck performs type checking 
 * by a postorder syntax tree traversal
 */
void typeCheck(TreeNode * syntaxTree)
{ traverse(syntaxTree,nullProc,checkNode);
}
