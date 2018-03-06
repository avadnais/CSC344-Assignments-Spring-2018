#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>

/* Andrew Vadnais
 * id: 805101869
 * CSC 344 - Prof. Dan Schlegel
 * SUNY Oswego
 * Modified 02-14-2018 */

typedef struct node
{
    char* folder;
    struct node* next;
} node;

// creates a new node
node* create(char* folder, node* next)
{
    node* new_node = (node*)malloc(sizeof(node));
    if(new_node == NULL)
    {
        printf("Error creating a new node.\n");
        exit(0);
    }

    new_node->folder = malloc(strlen(folder) + 1);
    strcpy(new_node->folder, folder);
    new_node->next = next;

    return new_node;
}

// adds a node to the list
node* append(node* head, char* folder)
{
    if(head == NULL)
        return NULL;
    /* go to the last node */
    node *cursor = head;
    while(cursor->next != NULL)
        cursor = cursor->next;

    /* create a new node */
    node* new_node =  create(folder, NULL);
    cursor->next = new_node;

    return head;
}

// returns all file names that start with input string
void fileComplete(node* cur, char* input) {
    int i = 1;
    while (cur != NULL) {
        int bool = 1;
        for (int i = 0; i < strlen(input); i++) {
            if ((int) input[i] != (int) cur->folder[i])
                bool = 0;
        }
        if (bool)
            printf("%s\n", cur->folder);
        cur = cur->next;
    }
    printf("\n");
}

int main() {

    const int MAX = 255; // max length of folder name
    printf("Enter a folder name: ");
    char* dir[MAX];
    scanf("%s", dir);

    node* ary[127]; // initialize an array of nodes

    DIR *dp;
    struct dirent *structdirent;
    dp = opendir (dir); // pointer to directory

    // create list
    if (dp != NULL)
    {
        // read in folders
        while ((structdirent = readdir (dp))) {
            char* encl;
            encl = malloc(sizeof(structdirent->d_name));
            strcpy(encl, structdirent->d_name);
            printf("%s", &encl);

            // iterate ary until first letter matches using ascii values
            for(int i = 0; i <= 122; ++i){
                if( (int) encl[0] == i){
                    // if linked list for the first letter as not been initialized, create a new node
                    if (ary[i] == NULL)
                        ary[i] = create(structdirent->d_name, NULL);
                    //else append to the list
                    else
                        append(ary[i], structdirent->d_name);
                }
            }
        }
        (void) closedir (dp);
    }
    else
        perror ("Directory not found");

    char *input; // first letters to search for
    input = (char *) malloc(sizeof(char) * MAX);

    //loop while user does not quit
    while(strcmp("quit", input)) {
        input = (char *) malloc(sizeof(char) * MAX);
        printf("> ");
        scanf("%s", input);
        node *cur = ary[(int) input[0]];
        printf("\nFiles starting with %s in %s:\n", input, (char*)dir);
        fileComplete(cur, input);

    }
    return 0;
}
