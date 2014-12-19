/****************************************************/
/* File: cgen.c                                     */
/* The code generator implementation                */
/* for the TINY compiler                            */
/* (generates code for the TM machine)              */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "code.h"
#include "cgen.h"

/* tmpOffset is the memory offset for temps
   It is decremented each time a temp is
   stored, and incremeted when loaded again
*/
static int tmpOffset = 0;
static int numberOfArguments = 0;

int forFunctionTable = 0;
int locMain;

static char* localNameStack[1024];
static int localNameStackIndex = 1024;
static char* parameterStack[1024];
static int parameterStackIndex = 1024;

/* prototype for internal recursive code generator */
static void cGen (TreeNode * tree);
static int pushArguments(int depth, TreeNode * tree);
static int pushParameters(char *name);
static int getLocalNameOffset(char *name);
static int getParameterOffset(char *name);
static void insertFunction(int functionLocation, char *name);
static void genExp( TreeNode * tree);
static void genStmt( TreeNode * tree);

/* Procedure genStmt generates code at a statement node */
void genStmt( TreeNode * tree)
{ TreeNode * p1, * p2, * p3;
  int savedLoc1,savedLoc2,currentLoc;
  int loc;
  char comment[128];
  int tmpSize = 0;
  if(tree == NULL)
    return;
  switch (tree->kind.stmt) {

      case IfK :
         if (TraceCode) emitComment("-> if start") ;
         p1 = tree->child[0] ;
         p2 = tree->child[1] ;
         p3 = tree->child[2] ;
         /* generate code for test expression */
         genExp(p1);
         savedLoc1 = emitSkip(1) ;
         emitComment("if: jump to else belongs here");
         /* recurse on then part */
         genStmt(p2);
         savedLoc2 = emitSkip(1) ;
         emitComment("if: jump to end belongs here");
         currentLoc = emitSkip(0) ;
         emitBackup(savedLoc1) ;
         emitRM_Abs("JEQ",ac,currentLoc,"if: jmp to else");
         emitRestore() ;
         /* recurse on else part */
         genStmt(p3);
         currentLoc = emitSkip(0) ;
         emitBackup(savedLoc2) ;
         emitRM_Abs("LDA",pc,currentLoc,"jmp to end") ;
         emitRestore() ;
         if (TraceCode)  emitComment("<- if end") ;
         break; /* if_k */

      case FunctionK:
         sprintf(comment, "-> function declaration %s", tree->attr.name);
         if (TraceCode) emitComment(comment);
         savedLoc1 = emitSkip(0);
         insertFunction(savedLoc1, tree->attr.name);
         if(strcmp(tree->attr.name, "main") == 0)
           locMain = savedLoc1;
         if(strcmp(tree->attr.name, "input") == 0)
           emitRO("IN",ac,0,0,"read integer value");
         else if(strcmp(tree->attr.name, "output") == 0)
         { emitRM("LD", ac, 1, fp, "load first argument");
           /* now output it */
           emitRO("OUT",ac,0,0,"write ac");
         }
         else
         {
           int numberOfParameters = pushParameters(tree->attr.name);
           genStmt(tree->child[1]);
           parameterStackIndex += numberOfParameters;
         }
         emitRM("LDA", mp, 0, fp, "copy fp to sp");
         emitRM("LD", fp, 0, mp, "pop fp");
         emitRM("LDC", ac1, 1, 0, "ac1 = 1");
         emitRO("ADD", mp, mp, ac1, "mp = mp + ac1");
         if(strcmp(tree->attr.name, "main") != 0) //메인이면 마지막 HALT로 
           emitRM("LD", pc, -2, mp, "jump to return address");
         sprintf(comment, "<- function declaration %s end", tree->attr.name);
         if (TraceCode) emitComment(comment);
         break;
      case CompoundK:
         sprintf(comment, "-> compound %d start", tree->lineno);
         if (TraceCode) emitComment(comment);
         p1 = tree->child[0];
         p2 = tree->child[1];
         tmpSize = 0;
         while(p1 != NULL)
         {
           if(p1->kind.exp == VarK)
           {
             tmpSize += 1;
             localNameStack[--localNameStackIndex] = p1->attr.name;
           }
           else
           {
             tmpSize += p1->child[0]->attr.val;
             localNameStackIndex -= p1->child[0]->attr.val;
             localNameStack[localNameStackIndex] = p1->attr.name;
           }
           p1 = p1->sibling;
         }
         emitRM("LDC", ac1, tmpSize, 0, "ac1 = sum of size of local variables");
         emitRO("SUB", mp, mp, ac1, "allocate local variables");
         cGen(p2);
         emitRM("LDC", ac1, tmpSize, 0, "ac1 = sum of size of local variables");
         emitRO("ADD", mp, mp, ac1, "free local variable");
         localNameStackIndex += tmpSize;
         sprintf(comment, "<- compound %d end", tree->lineno);
         if (TraceCode) emitComment(comment);
         break;
      case WhileK:
         if (TraceCode) emitComment("-> while start") ;
         p1 = tree->child[0];
         p2 = tree->child[1];
         savedLoc1 = emitSkip(0);
         if (TraceCode) emitComment("while : test expression start");
         genExp(p1);
         if (TraceCode) emitComment("while : test expression end");
         savedLoc2 = emitSkip(1);
         if (TraceCode) emitComment("while : body start");
         genStmt(p2);
         if (TraceCode) emitComment("while : body end");
         emitRM("LDC", pc, savedLoc1, 0, "unconditional jump");
         currentLoc = emitSkip(0);
         emitBackup(savedLoc2);
         emitRM_Abs("JEQ", ac, currentLoc, "while : false");
         emitRestore();
         break;
      case ReturnK:
         if(tree->child[0] != NULL)
           genExp(tree->child[0]);
         break;
      default:
         break;
    }
} /* genStmt */

/* Procedure genExp generates code at an expression node */
void genExp( TreeNode * tree)
{ int loc, depth, arrayIndex;
  TreeNode * p1, * p2;
  char comment[128];
  int isArray = 0;
  if(tree == NULL)
    return;
  switch (tree->kind.exp) {

    case ConstK :
      sprintf(comment, "-> const %d", tree->attr.val);
      if (TraceCode) emitComment(comment) ;
      /* gen code to load integer constant using LDC */
      emitRM("LDC",ac,tree->attr.val,0,"load const");
      if (TraceCode)  emitComment("<- Const end") ;
      break; /* ConstK */
    
    case IdArrayK :
      if (TraceCode) emitComment("-> array") ;
      emitRM("LDC", ac1, 0, 0, "");
      loc = getLocalNameOffset(tree->attr.name);
      if(loc == -1)
      {
        loc = getParameterOffset(tree->attr.name);
        if(loc == -1)
        {
          loc = st_get_location("~", tree->attr.name);
          emitRM("LDA", ac, loc, gp, "id : load address to ac");
        }
        else
          emitRM("LD", ac, loc + 1, fp, "id : load address to ac");
      }
      else
        emitRM("LDA", ac, loc, mp, "id : load address to ac");
      if(tree->child[0] != NULL)
      {
        emitRM("ST",ac,--tmpOffset,mp,"op: push ac"); 
        genExp(tree->child[0]);
        emitRM("LDA", ac1, 0, ac, "save index to ac1");
        emitRM("LD",ac,tmpOffset++,mp, "op: load ac");
        emitRO("ADD", ac1, ac1, ac, "get location");
        emitRM("LD", ac, 0, ac1, "get value");
      }
      break;
    case IdK :
      if (TraceCode) emitComment("-> Id") ;
      loc = getLocalNameOffset(tree->attr.name);
      if (loc == -1) //parameter, global
      {
        loc = getParameterOffset(tree->attr.name);
        if (loc == -1)
        {
          loc = st_get_location("~", tree->attr.name);
          emitRM("LD", ac, loc, gp, "id: load value to ac");
        }
        else
          emitRM("LD", ac, loc + 1, fp, "id: load value to ac");
      }
      else
        emitRM("LD", ac, loc, mp, "id: load value to ac");
      if (TraceCode)  emitComment("<- Id") ;
      break; /* IdK */

    case OpK :
         if (TraceCode) emitComment("-> Op") ;
         p1 = tree->child[0];
         p2 = tree->child[1];
         /* gen code for ac = left arg */
         if (TraceCode) emitComment("-> left") ;
         genExp(p1);
         if (TraceCode) emitComment("<- left") ;
         /* gen code to push left operand */
         emitRM("ST",ac,--tmpOffset,mp,"op: push left");
         /* gen code for ac = right operand */
         if (TraceCode) emitComment("-> right") ;
         genExp(p2);
         if (TraceCode) emitComment("<- right") ;
         /* now load left operand */
         emitRM("LD",ac1,tmpOffset++,mp,"op: load left");
         switch (tree->attr.op) {
            case PLUS :
               emitRO("ADD",ac,ac1,ac,"op +");
               break;
            case MINUS :
               emitRO("SUB",ac,ac1,ac,"op -");
               break;
            case TIMES :
               emitRO("MUL",ac,ac1,ac,"op *");
               break;
            case OVER :
               emitRO("DIV",ac,ac1,ac,"op /");
               break;
            case LT :
               emitRO("SUB",ac,ac1,ac,"op <") ;
               emitRM("JLT",ac,2,pc,"br if true") ;
               emitRM("LDC",ac,0,ac,"false case") ;
               emitRM("LDA",pc,1,pc,"unconditional jmp") ;
               emitRM("LDC",ac,1,ac,"true case") ;
               break;
            case LE :
               emitRO("SUB",ac,ac1,ac,"op <=") ;
               emitRM("JLE",ac,2,pc,"br if true") ;
               emitRM("LDC",ac,0,ac,"false case") ;
               emitRM("LDA",pc,1,pc,"unconditional jmp") ;
               emitRM("LDC",ac,1,ac,"true case") ;
               break;
            case GT :
               emitRO("SUB",ac,ac1,ac,"op >") ;
               emitRM("JGT",ac,2,pc,"br if true") ;
               emitRM("LDC",ac,0,ac,"false case") ;
               emitRM("LDA",pc,1,pc,"unconditional jmp") ;
               emitRM("LDC",ac,1,ac,"true case") ;
               break;
            case GE :
               emitRO("SUB",ac,ac1,ac,"op >=") ;
               emitRM("JGE",ac,2,pc,"br if true") ;
               emitRM("LDC",ac,0,ac,"false case") ;
               emitRM("LDA",pc,1,pc,"unconditional jmp") ;
               emitRM("LDC",ac,1,ac,"true case") ;
               break;
            case EQ :
               emitRO("SUB",ac,ac1,ac,"op ==") ;
               emitRM("JEQ",ac,2,pc,"br if true");
               emitRM("LDC",ac,0,ac,"false case") ;
               emitRM("LDA",pc,1,pc,"unconditional jmp") ;
               emitRM("LDC",ac,1,ac,"true case") ;
               break;
            case NE :
               emitRO("SUB",ac,ac1,ac,"op !=") ;
               emitRM("JNE",ac,2,pc,"br if true");
               emitRM("LDC",ac,0,ac,"false case") ;
               emitRM("LDA",pc,1,pc,"unconditional jmp") ;
               emitRM("LDC",ac,1,ac,"true case") ;
               break;
            default:
               emitComment("BUG: Unknown operator");
               break;
         } /* case op */
         if (TraceCode)  emitComment("<- Op") ;
         break; /* OpK */
    case VarK:
      break;
    case VarArrayK:
      break;
    case CallK:
      sprintf(comment, "-> call function %s", tree->attr.name);
      if(TraceCode) emitComment(comment);
      p1 = tree->child[0];
      numberOfArguments = pushArguments(0, p1);
      emitRM("LDA", mp, -numberOfArguments, mp, "stack growth after push arguments");
      tmpOffset = 0;
      sprintf(comment, "%d arguments are pushed", numberOfArguments);
      if(TraceCode) emitComment(comment);
      emitRM("LDC", ac1, 1, 0, "ac1 = 1");
      emitRO("SUB", mp, mp, ac1, "mp = mp - ac1");
      emitRM("ST", fp, 0, mp, "push fp");
      emitRM("LDA", fp, 0, mp, "copy sp to fp");
      emitRO("SUB", mp, mp, ac1, "mp = mp - ac1");
      emitRM("LDC", ac1, 2, 0, "ac1 = 2");
      emitRO("ADD", ac1, ac1, pc, "calculate return address");
      emitRM("ST", ac1, 0, mp, "push return address");
      loc = st_get_location("~", tree->attr.name);
      if (TraceCode) sprintf(comment, "jump to function at %d", loc);
      emitRM("LD", pc, loc, gp, comment);
      if (numberOfArguments > 0)
      {
         emitRM("LDC", ac1, numberOfArguments, 0, "ac1 = numberOfArguments");
         emitRO("ADD", mp, mp, ac1, "pop arguments");
      }
      sprintf(comment, "<- call function %s end", tree->attr.name);
      break;
    case AssignK:
      sprintf(comment, "-> assign to %s", tree->child[0]->attr.name);
      if (TraceCode) emitComment(comment) ;
      /* generate code for rhs */
      if (TraceCode) emitComment("-> generate code for rhs") ;
      genExp(tree->child[1]);
      if (TraceCode) emitComment("<- generate code for rhs end") ;
      /* now store value */
      if (TraceCode) emitComment("-> store value start") ;
      if (tree->child[0]->kind.exp == IdArrayK)
      {
        isArray  = 1;
        if (TraceCode) emitComment("-> array") ;
        emitRM("ST",ac,--tmpOffset,mp,"op: push ac");        
        genExp(tree->child[0]->child[0]);
        emitRM("LDA", ac1, 0, ac, "save index to ac1");
        loc = getLocalNameOffset(tree->child[0]->attr.name);
        if (loc == -1)
        {
          loc = getParameterOffset(tree->child[0]->attr.name);
          if (loc == -1)
          {
            loc = st_get_location("~", tree->child[0]->attr.name);
            emitRM("LDA", ac, loc, gp, "load address");
            emitRO("ADD", ac1, ac, ac1, "ac1 = address + index");
          }
          else
          {
            emitRM("LD", ac, loc + 1, fp, "load address");
            emitRO("ADD", ac1, ac, ac1, "ac1 = address + index");
          }
        }
        else
        {
          emitRM("LDA", ac, loc, mp, "load address");
          emitRO("ADD", ac1, ac, ac1, "ac1 = address + index");
        }
        emitRM("LD",ac,tmpOffset++,mp,"op: load ac");
        emitRM("ST", ac, 0, ac1, "store");
        if (TraceCode) emitComment("<- store value end") ;
        if (TraceCode)  emitComment("<- assign") ;
        break;
      }
      loc = getLocalNameOffset(tree->child[0]->attr.name);
      if (loc == -1) //parameter, global
      {
        loc = getParameterOffset(tree->child[0]->attr.name);
        if (loc == -1)
        {
          loc = st_get_location("~", tree->child[0]->attr.name);
          //if(isArray == 1) emitRO("ADD", gp, gp, ac1, "to access array");
          emitRM("ST", ac, loc, gp, "assign: store value");
          //if(isArray == 1) emitRO("SUB", gp, gp, ac1, "to access array");
        }
        else
        {
          //if(isArray == 1) emitRO("ADD", fp, fp, ac1, "to access array");
          emitRM("ST", ac, loc + 1, fp, "assign: store value");
          //if(isArray == 1) emitRO("SUB", fp, fp, ac1, "to access array");
        }
      }
      else
      { //if(isArray == 1) emitRO("ADD", mp, mp, ac1, "to access array");
        emitRM("ST", ac, loc, mp, "assign: store value");
        //if(isArray == 1) emitRO("SUB", mp, mp, ac1, "to access array");
      }
      if (TraceCode) emitComment("<- store value end") ;
      if (TraceCode)  emitComment("<- assign") ;
      break; /* assign_k */
    case SingleParamK:
      break;
    case ArrayParamK:
      break;
    default:
      break;
  }
} /* genExp */

/* Procedure cGen recursively generates code by
 * tree traversal
 */
static void cGen( TreeNode * tree)
{ if (tree != NULL)
  { switch (tree->nodekind) {
      case StmtK:
        genStmt(tree);
        break;
      case ExpK:
        genExp(tree);
        break;
      default:
        break;
    }
    cGen(tree->sibling);
  }
}

int getSizeOfGlobal(TreeNode * syntaxTree)
{
   int result = 0;
   TreeNode *tree = syntaxTree;
   while(tree != NULL)
   {
     if(tree->nodekind == ExpK && tree->kind.stmt == VarArrayK)
       result += tree->child[0]->attr.val;
     else
       result++;
     tree = tree->sibling;
   }
   return result;
}

/**********************************************/
/* the primary function of the code generator */
/**********************************************/
/* Procedure codeGen generates code to a code
 * file by traversal of the syntax tree. The
 * second parameter (codefile) is the file name
 * of the code file, and is used to print the
 * file name as a comment in the code file
 */
void codeGen(TreeNode * syntaxTree, char * codefile)
{  char * s = malloc(strlen(codefile)+7);
   strcpy(s,"File: ");
   strcat(s,codefile);
   emitComment("TINY Compilation to TM Code");
   emitComment(s);
   /* generate standard prelude */
   emitComment("Standard prelude:");
   emitRM("LD",mp,0,ac,"load maxaddress from location 0");
   emitRM("ST",ac,0,ac,"clear location 0");
   emitComment("End of standard prelude.");
   /* generate code for TINY program */
   forFunctionTable = emitSkip(getSizeOfGlobal(syntaxTree)*2 + 1);
   cGen(syntaxTree);
   /* jump to main */
   int memloc = st_get_location("~", "main");
   emitBackup(forFunctionTable);
   emitRM("LDC", pc, locMain, 0, "jump to main");
   emitRestore();
   /* finish */
   emitComment("End of execution.");
   emitRO("HALT",0,0,0,"done");
}

int pushArguments(int depth, TreeNode * tree)
{
   if(tree == NULL)
    return depth;
   depth = pushArguments(depth + 1, tree->sibling);
   genExp(tree);
   //parameterStack[--parameterStackIndex] = tree->attr.name;
   //emitRM("LDC", ac1, 1, 0, "ac1 = 1");
   //emitRO("SUB", mp, mp, ac1, "mp = mp - ac1");
   emitRM("ST", ac, --tmpOffset, mp, "op: push argument(reverse order)");
   return depth;
}

int pushParameters(char *functionName)
{
   char* parameters[SIZE];
   char tmp[128];
   int max = 0;
   sprintf(tmp, "~:%s", functionName);
   ScopeList scope = getScope(tmp);
   if(scope == NULL)
     return;
   int i;
   for (i=0;i<SIZE;++i)
   { if (scope->bucket[i] != NULL)
     { BucketList l = scope->bucket[i];
       while (l != NULL)
       {
         if(max < l->memloc)
           max = l->memloc;
         parameters[l->memloc] = l->name;
         l = l->next;
       }
     }
   }

   for (i=max; i>=0;i--)
     parameterStack[--parameterStackIndex] = parameters[i];

   return max + 1;
}

void insertFunction(int functionLocation, char *name)
{
   char comment[128];
   int memloc = st_get_location("~", name);
   emitBackup(forFunctionTable);
   forFunctionTable += 2;
   sprintf(comment, "function %s is at %d", name, memloc);
   if (TraceCode) emitComment(comment);
   sprintf(comment, "load function location(%d)", functionLocation);
   emitRM("LDC", ac, functionLocation, 0, comment);
   emitRM("ST", ac, memloc, gp, "add into memory");
   emitRestore();
}

int getLocalNameOffset(char *name)
{
   int i;
   for(i = localNameStackIndex; i < 1024; i++)
     if(localNameStack[i] != 0 && strcmp(localNameStack[i], name) == 0)
       return i - localNameStackIndex;
   return -1;
}

int getParameterOffset(char *name)
{
   int i;
   for(i = parameterStackIndex; i< 1024; i++)
     if(parameterStack[i] != 0 && strcmp(parameterStack[i], name) == 0)
       return i - parameterStackIndex;
   return -1;
}