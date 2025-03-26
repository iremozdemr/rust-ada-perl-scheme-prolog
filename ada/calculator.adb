with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;

procedure Simple_Calculator is
   A, B, Result : Float;
   Operator     : Character;
begin
   Put("enter the first number: ");
   Get(A);

   Put("enter the second number: ");
   Get(B);

   Put("enter the operator (+, -, *, /): ");
   Get(Operator);

   if Operator = '+' then
      Result := A + B;
   elsif Operator = '-' then
      Result := A - B;
   elsif Operator = '*' then
      Result := A * B;
   elsif Operator = '/' then
      if B = 0.0 then
         Put_Line("error: division by zero");
         return;
      else
         Result := A / B;
      end if;
   else
      Put_Line("error");
      return;
   end if;

   Put("result: ");
   Put(Result, Fore => 1, Aft => 2, Exp => 0);
   New_Line;
end Simple_Calculator;