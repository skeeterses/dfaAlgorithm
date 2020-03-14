with Ada.Text_IO;

package body dfa is

    package U_Operator_IO is new Ada.Text_IO.Enumeration_IO(U_Operator);
    package B_Operator_IO is new Ada.Text_IO.Enumeration_IO(B_Operator);

    procedure PrintNode(inputNode : SyntaxNode) is
    begin
        case inputNode.inputType is
	    when UnaryOperator =>
    	        Ada.Text_IO.Put("Value: ");
		U_Operator_IO.Put(inputNode.U_value);
		Ada.Text_IO.New_Line;
                -- A unary operator MUST have a child node, ex in a*, * is
		-- the operator and 'a' is the child.
		Ada.Text_IO.Put("Child node: ");
		PrintNode(inputNode.SubNode.all);
    
	   when BinaryOperator =>
		Ada.Text_IO.Put("Value: ");
		B_Operator_IO.Put(inputNode.B_value);
		Ada.Text_IO.New_Line;
                --Binary operator has 2 children:  A|B has | as the value,
		--   A as the left child, and B as the right child.
                Ada.Text_IO.Put("Right child of ");
		B_Operator_IO.Put(inputNode.B_value);
		Ada.Text_IO.Put(":");
		PrintNode(inputNode.RightNode.all);
		Ada.Text_IO.New_Line;

                Ada.Text_IO.Put("Left child of ");
		B_Operator_IO.Put(inputNode.B_Value);
		Ada.Text_IO.Put(":");
		PrintNode(inputNode.LeftNode.all);
		Ada.Text_IO.New_Line;

	   when ascii =>
		Ada.Text_IO.Put("Value: ");
		Ada.Text_IO.Put(inputNode.A_value);
		Ada.Text_IO.New_Line;
         end case;	     
    end PrintNode;

end dfa;
