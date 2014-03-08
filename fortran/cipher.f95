!Caesar Cipher - Fortran
!Author: John Paul Welsh

program cipher
implicit none

character :: input_string*15
integer   :: shift_amt, max_shift_value

input_string='John Paul Welsh'
shift_amt=-19
max_shift_value=28

print *, 'Original string is ',input_string
print *, 'The shift amount for encrypt and decrypt is ',shift_amt
print *, 'The max shift value for solve is ',max_shift_value

shift_amt=MOD(shift_amt,26)

call capitalize(input_string)
call encrypt(input_string, shift_amt)
call decrypt(input_string, shift_amt)
call solve(input_string, max_shift_value)

end program cipher

!++++++++++++++++++++++++++++++
subroutine capitalize(input_string)
implicit none

character :: input_string*15, ch*1
integer   :: string_pos, code

do string_pos=1,15
    ch=input_string(string_pos:string_pos)
    code=iachar(ch)
    
    if (code .NE. 32) then
        if (code < 123 .AND. code > 96) then
            code=code-32
            input_string(string_pos:string_pos)=achar(code)
        end if
    end if
end do

end subroutine capitalize
!++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++
subroutine encrypt(input_string, shift_amt)
implicit none

character :: input_string*15, output_string*15, ch*1
integer   :: string_pos, code, shift_amt

do string_pos=1,15
    ch=input_string(string_pos:string_pos)
    code=iachar(ch)
    
    if (code .NE. 32) then
        code=code+shift_amt
        
        if (code > 90) then
            code=64+MOD(code,90)
        end if
        if (code < 65) then
            code=91-MOD(65,code)
        end if
        
    end if
    
    output_string(string_pos:string_pos)=achar(code)
end do

print *, 'Encrypt: ',output_string

end subroutine encrypt
!++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++
subroutine decrypt(input_string, shift_amt)
implicit none

character :: input_string*15, output_string*15, ch*1
integer   :: string_pos, code, shift_amt

do string_pos=1,15
    ch=input_string(string_pos:string_pos)
    code=iachar(ch)
    
    if (code .NE. 32) then
        code=code-shift_amt
        
        if (code > 90) then
            code=64+MOD(code,90)
        end if
        if (code < 65) then
            code=91-MOD(65,code)           
        end if
        
    end if
    
    output_string(string_pos:string_pos)=achar(code)
end do

print *, 'Decrypt: ',output_string

end subroutine decrypt
!++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++
subroutine solve(input_string, max_shift_value)
implicit none

character :: input_string*15, output_string*15, ch*1
integer   :: string_pos, code, max_shift_value, shift_amt, iter_count

do iter_count=max_shift_value,0,-1
    shift_amt=iter_count

    do string_pos=1,15

        ch=input_string(string_pos:string_pos)
        code=iachar(ch)
    
        if (code .NE. 32) then
            code=code+shift_amt
            
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
