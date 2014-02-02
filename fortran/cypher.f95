!Caesar Cypher
!Author: John Paul Welsh

!codes are 65..90 for A..Z

program cypher
implicit none

character :: input_string*15, output_string*15
integer :: string_pos, shift_amt

input_string='JOHN PAUL WELSH'
shift_amt=3
output_string=''

!return output_string and print it
call encrypt(input_string, string_pos, shift_amt)

call decrypt(input_string, string_pos, shift_amt)

end program cypher

!++++++++++++++++++++++++++++++
subroutine encrypt(input_string, string_pos, shift_amt)
implicit none

character :: input_string*15, output_string*15, ch*1
integer :: string_pos, code, shift_amt, shift_count

do string_pos=1,15

    ch=input_string(string_pos:string_pos)
    code=iachar(ch)
    
    print *, 'Char = ', ch, ', code = ', code

    !do not shift whitespace
    if (code .NE. 32) then
        do shift_count=1, shift_amt
            code=code+1
        end do
    end if
    
    output_string(string_pos:string_pos)=achar(code)
    
    print *, 'After encrypt shift'
    print *, 'Char = ', ch, ', code = ', code
    print *
end do

print *, output_string

end subroutine encrypt
!++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++
subroutine decrypt(input_string, string_pos, shift_amt)
implicit none

character :: input_string*15, output_string*15, ch*1
integer :: string_pos, code, shift_amt, shift_count

do string_pos=1,15

    ch=input_string(string_pos:string_pos)
    code=iachar(ch)
    
    print *, 'Char = ', ch, ', code = ', code

    !do not shift whitespace
    if (code .NE. 32) then
        do shift_count=1, shift_amt
            code=code-1
        end do
    end if
    
    output_string(string_pos:string_pos)=achar(code)
    
    print *, 'After decrypt shift'
    print *, 'Char = ', ch, ', code = ', code
    print *
end do

print *, output_string

end subroutine decrypt
!++++++++++++++++++++++++++++++