with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;

procedure Simple_Calculator is
   A, B, Result : Float;
   Operator     : Character;
begin
   Put("Birinci sayıyı girin: ");
   Get(A);

   Put("İkinci sayıyı girin: ");
   Get(B);

   Put("İşlem türünü girin (+, -, *, /): ");
   Get(Operator);

   if Operator = '+' then
      Result := A + B;
   elsif Operator = '-' then
      Result := A - B;
   elsif Operator = '*' then
      Result := A * B;
   elsif Operator = '/' then
      if B = 0.0 then
         Put_Line("Hata: Sıfıra bölme yapılamaz.");
         return;
      else
         Result := A / B;
      end if;
   else
      Put_Line("Geçersiz işlem!");
      return;
   end if;

   Put("Sonuç: ");
   Put(Result, Fore => 1, Aft => 2, Exp => 0);
   New_Line;
end Simple_Calculator;