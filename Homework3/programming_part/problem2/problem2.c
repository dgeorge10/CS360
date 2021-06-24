#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define FAILED NULL
typedef struct NODE* TREE;

struct NODE {
	char label;
	TREE leftmostChild, rightSibling;
};

TREE makeNode0(char x);
TREE makeNode1(char x, TREE t);
TREE makeNode4(char x, TREE t1, TREE t2, TREE t3, TREE t4);
TREE B(char match);
TREE parseTree; /* holds the result of the parse */
char *nextTerminal; /* current position in input string */
int computeHeight(TREE tree);
void printPreOrder(TREE tree);
void printPostOrder(TREE tree);
void main(int argc, char *argv[])
{
	char *tests[] = {"1010", "0110", "01", "10", "11110000", "0001100", "1111111"};
	for(int i = 0; i < 7; i++){
		char* cur = tests[i];
		printf("current test string: %s\n", cur);
		nextTerminal = cur;
		parseTree = B('\0');

		if(parseTree!=NULL){
			int h = computeHeight(parseTree);
			printf("height: %d\n", h);
			printf("preorder:\n");
			printPreOrder(parseTree);
			printf("\n");
			printf("postorder:\n");
			printPostOrder(parseTree);
			printf("\n");
		} else 
			printf("failed to make tree\n");
		printf("\n");
	}

}
int computeHeight(TREE tree)
{
	if(tree==NULL)
		return -1;
	else{
		int myHeight = 1 + computeHeight(tree->leftmostChild);
		int rightSiblingHeight = computeHeight(tree->rightSibling);
		if(myHeight > rightSiblingHeight)
			return myHeight;
		else return rightSiblingHeight;
	}
}
void printPreOrder(TREE tree)
{
	if(tree!=NULL){
		printf("%c ", tree->label);
		printPreOrder(tree->leftmostChild);
		printPreOrder(tree->rightSibling);
	}
}
void printPostOrder(TREE tree)
{
	if(tree!=NULL){
		printPostOrder(tree->leftmostChild);
		printPostOrder(tree->rightSibling);
		printf("%c ", tree->label);
	}
}
TREE makeNode0(char x)
{
	TREE root;
	root = (TREE) malloc(sizeof(struct NODE));
	root->label = x;
	root->leftmostChild = NULL;
	root->rightSibling = NULL;
	return root;
}
TREE makeNode1(char x, TREE t)
{
	TREE root;
	root = makeNode0(x);
	root->leftmostChild = t;
	return root;
}
TREE makeNode4(char x, TREE t1, TREE t2, TREE t3, TREE t4)
{
	TREE root;
	root = makeNode1(x, t1);
	t1->rightSibling = t2;
	t2->rightSibling = t3;
	t3->rightSibling = t4;
	return root;
}

TREE B(char needToMatch)
{
	TREE firstB, secondB;
	if(needToMatch == *nextTerminal) /* follow production 1 */ {
		return makeNode1('B', makeNode0('e'));
	}
	else if(*nextTerminal == '0') /* follow production 2 */ {
		nextTerminal++;
		firstB = B('1');
		if(firstB != FAILED && *nextTerminal == '1') {
			nextTerminal++;
			secondB = B(needToMatch);
			if(secondB == FAILED)
				return FAILED;
			else
				return makeNode4('B',
						makeNode0('0'),
						firstB,
						makeNode0('1'),
						secondB);
		}
		else /* first call to B failed */
			return FAILED;
	}
	else if(*nextTerminal == '1') { /* follow production 3*/
		nextTerminal++;
		firstB = B('0');
		if(firstB != FAILED && *nextTerminal == '0') {
			nextTerminal++;
			secondB = B(needToMatch);
			if(secondB == FAILED)
				return FAILED;
			else
				return makeNode4('B',
						makeNode0('1'),
						firstB,
						makeNode0('0'),
						secondB);
		}
		else /* first call to B failed */
			return FAILED;
	}
	else /* follow production 1 */
		return makeNode1('B', makeNode0('e'));
}

