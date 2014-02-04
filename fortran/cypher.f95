!Caesar Cypher
!Author: John Paul Welsh

program cypher

implicit none

character :: input_string*15
integer :: shift_amt, max_shift_value

input_string='JOHN PAUL WELSH'
shift_amt=12
max_shift_value=26

call encrypt(input_string, shift_amt)

call decrypt(input_string, shift_amt)

call solve(input_string, max_shift_value)

end program cypher

!++++++++++++++++++++++++++++++
subroutine encrypt(input_string, shift_amt)
implicit none

character :: input_string*15, output_string*15, ch*1
integer :: string_pos, code, shift_amt, shift_count

do string_pos=1,15

    ch=input_string(string_pos:string_pos)
    code=iachar(ch)
    
    if (code .NE. 32) then
        do shift_count=1, shift_amt
            code=code+1
        end do
        if (code > 90) then
            code=64+MOD(code,90)
        end if
        if (code < 65) then
            code=code-MOD(65,code)
        end if
    end if
    
    output_string(string_pos:string_pos)=achar(code)
end do

print *, output_string

end subroutine encrypt
!++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++
subroutine decrypt(input_string, shift_amt)
implicit none

character :: input_string*15, output_string*15, ch*1
integer :: string_pos, code, shift_amt, shift_count

do string_pos=1,15

    ch=input_string(string_pos:string_pos)
    code=iachar(ch)
    
    if (code .NE. 32) then
        do shift_count=1, shift_amt
            code=code-1
        end do
        if (code > 90) then
            code=64+MOD(code,90)
        end if
        if (code < 65) then
            code=91-MOD(65,code)           
        end if
    end if
    
    output_string(string_pos:string_pos)=achar(code)
end do

print *, output_string

end subroutine decrypt
!++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++
subroutine solve(input_string, max_shift_value)
implicit none

character :: input_string*15, output_string*15, ch*1
integer :: string_pos, code, max_shift_value, shift_count, shift_amt
integer :: iter_count

do iter_count=max_shift_value,0,-1

    shift_amt=iter_count

    do string_pos=1,15

        ch=input_string(string_pos:string_pos)
        code=iachar(ch)
    
        if (code .NE. 32) then
            do shift_count=1, shift_amt
                code=code+1
            end do
            if (code > 90) then
                code=64+MOD(code,90)
            end if
            if (code < 65) then
                code=91-MOD(65,code)            
            end if
        end if

        output_string(string_pos:string_pos)=achar(code)
    end do
    
    print *, 'Caesar ',iter_count,': ',output_string
end do

end subroutine solve
!++++++++++++++++++++++++++++++