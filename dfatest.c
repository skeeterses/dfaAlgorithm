#include <stdio.h>

extern unsigned char yy_next(int state, unsigned int c);

void main()
{
/*  A simple test to make sure the transition tables work.
   The test transition tables are in ExampleFSM1.c ExampleFSM2.c, and
      ExampleFSM3.c
   printf("A state transition failure: ");
   printf("%d", (signed char) yy_next(1, 'a'));
   printf("\n");
   printf("A state transition success: ");
   printf("%d", (signed char) yy_next(1, '.'));
   printf("\n");
*/

   char testString[20];
   int menuOption = 0;
   signed char current_state;

   while (menuOption != 2)
   {
      printf("FSM table testing program.\n");
      printf("Enter 1 to test a string.\n");
      printf("      2  to quit.\n");
      scanf("%d", &menuOption);

      if (menuOption == 1)
      {
      //Run the FSM table test
	//When running the DFAmaker code, this file and especially this
	// portion will be generated automatically

	printf("Enter in test string now:\n");
	scanf("%s", &testString);

	char *position;
        current_state = 0;
	position = testString;
        while (*position != '\0')
	{
           current_state = yy_next(current_state, *position);
	   position++;
	}	
        printf("Current state is now %d\n", current_state);
      }
   }
}
