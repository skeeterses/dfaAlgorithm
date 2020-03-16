with ada.text_io;
with Ada.integer_text_io;

with dfa;

procedure dfamaker is 
  RegularExpression : String(1..30);
  ExpressionName : String(1..20);
  MenuOption : Integer := 0;
  Last1, Last2 : Natural;

FSMType : Integer := 0;		
  -- The Default FSM will be an uncompressed table.
begin
   Ada.Text_IO.Put("Welcome to the DFA maker test program.");
   Ada.Text_IO.New_Line;
 
   while MenuOption /= 2 loop
      Ada.Text_IO.Put("Enter 1 to input a regular expression.");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("      2 to exit.");
      Ada.Text_IO.New_line;

--Skip_Line is necessary for reading multiple strings
      Ada.Integer_Text_IO.get(Menuoption);
      if MenuOption = 1 then
         Ada.Text_IO.Put("Enter regular expression and token name");
	 Ada.Text_IO.New_Line;
	 Ada.Text_IO.skip_line;
     	 Ada.Text_IO.get_line(Item => RegularExpression, Last => Last1);
	 Ada.Text_IO.get_line(Item => ExpressionName, Last => Last2);         

	 Ada.Text_IO.Put_line(regularexpression(1..Last1));
	 Ada.Text_IO.Put_line(expressionname(1..Last2));

      end if;

   end loop;


end dfamaker;
