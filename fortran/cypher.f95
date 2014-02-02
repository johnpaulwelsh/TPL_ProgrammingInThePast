!Caesar Cypher
!Author: John Paul Welsh
program cypher
implicit none

character :: input_string*15, output_string*15
integer :: string_pos, shift_amt, shift_count

input_string='JOHNPAULWELSH'
shift_amt=3
output_string=' '

call encrypt(input_string, string_pos)

end program cypher

!++++++++++++++++++++++++++++++
subroutine encrypt(input_string, string_pos)
implicit none

character :: input_string*15
integer :: string_pos

!iterate through input_string
do string_pos=1,15

    print *, input_string(string_pos:string_pos)

end do

end subroutine encrypt
!++++++++++++++++++++++++++++++

!===============
!iterate through input_string
do string_pos=1, *string length*
!iachar(c) = get ASCII code "i" for the char "c"

    !increment i "shift_amt" times
    do shift_count=1,shift_amt
        output_string(shift_count:shift_count) = input_string()
    
    
    end do
!achar(i) = get char "c" with the ASCII code "i"
end do
!===============