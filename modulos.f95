module ordenar

    contains 
!_________________________________________________________________________________________________
    subroutine ord_intercambio(X,n)
        integer :: i, j, aux
        integer :: X(n)

        X=(/2,0,6,5,3,1,8,4/)

        do i=1,n                     !Recorremos la lista de numeros
            aux=X(i)                 !Empezamos con el primer elemento
            do j=1,n 
                if(aux<X(j)) then    !Comprueba el primer numero de la lista con los demas
                    aux=X(i)
                    X(i)=X(j)        !Si el numero de la izquierda es mayor, se permutan
                    X(j)=aux
                end if
            end do
            write(unit=10, fmt='(8(I4))', iostat=ierr) X
        end do
    end subroutine ord_intercambio
    
end module