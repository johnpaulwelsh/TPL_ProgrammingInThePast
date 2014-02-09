Program cypher;

Var input : String;
Var shiftAmount, maxShiftVal: Integer;

{-------------------------------------------------------}
Procedure encrypt(input : String; shiftAmount : Integer);

Var output : String;
Var strPos, ascii, charCount : Integer;

Begin
    output := '';

    For strPos := 1 to 15 do
    Begin
        ascii := ord(input[strPos]);
        
        if not(ascii = 32) then
        Begin
            if (shiftAmount < 0) then
                For charCount := -1 DOWNTO shiftAmount do ascii := ascii - 1
            else
                For charCount := 1 to shiftAmount do ascii := ascii + 1;
            if (ascii > 90) then
                ascii := 64 + (ascii mod 90);
            if (ascii < 65) then
                ascii := 91 - (65 mod ascii);
        End;
        
        output := output + chr(ascii);
    End;
    
    Writeln(output);
End;
{-------------------------------------------------------}

{-------------------------------------------------------}
Procedure decrypt(input : String; shiftAmount : Integer);

Var output : String;
Var strPos, ascii, charCount : Integer;

Begin
    output := '';

    For strPos := 1 to 15 do
    Begin
        ascii := ord(input[strPos]);
        
        if not(ascii = 32) then
        Begin
            if (shiftAmount < 0) then
                For charCount := -1 DOWNTO shiftAmount do ascii := ascii + 1
            else
                For charCount := 1 to shiftAmount do ascii := ascii - 1;
        
            if (ascii > 90) then
                ascii := 64 + (ascii mod 90);
            if (ascii < 65) then
                ascii := 91 - (65 mod ascii);
        End;
        
        output := output + chr(ascii);
    End;
    
    Writeln(output);
End;
{-------------------------------------------------------}

{-------------------------------------------------------}
Procedure solve(input : String; maxShiftVal : Integer);

Var output : String;
Var strPos, ascii, charCount, shiftAmount, iteration : Integer;

Begin
    For iteration := maxShiftVal DOWNTO 0 do
    Begin
        output := '';
        shiftAmount := iteration;
        
        For strPos := 1 to 15 do
        Begin
            ascii := ord(input[strPos]);
            
            if not(ascii = 32) then
            Begin
                For charCount := 1 to shiftAmount do ascii := ascii + 1;
            
                if (ascii > 90) then
                    ascii := 64 + (ascii mod 90);
                if (ascii < 65) then
                    ascii := 91 - (65 mod ascii);
            End;
            
            output := output + chr(ascii);
        End;
        
        Writeln('Caesar ', iteration, ': ', output);
    End;
End;
{-------------------------------------------------------}

{-------------------------------------------------------}
Begin
    input := 'John Paul Welsh';
    shiftAmount := 30;
    maxShiftVal := 26;
    
    Writeln('The input string is ', input);
    Writeln('The shift amount for encrypt and decrypt is ', shiftAmount);
    Writeln('The maximum shift value for solve is ', maxShiftVal);
    
    input := upcase(input);
    
    if (shiftAmount > 26) then
        shiftAmount := shiftAmount mod 26;
    
    encrypt(input, shiftAmount);
    decrypt(input, shiftAmount);
    solve(input, maxShiftVal);
End.
{-------------------------------------------------------}