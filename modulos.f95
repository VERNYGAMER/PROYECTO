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

!_________________________________________________________________________________________________
    subroutine ord_seleccion(X,n) 
        integer :: i, j, aux, min
        integer :: X(n)

        X=(/2,0,6,5,3,1,8,4/)

        do i=1,n-1       !Recorremos todas las posiciones
            min=i        !Tomamos como minimo la primera posicion
            do j=i+1,n   !Buscamos el minimo a la derecha de i
                if (X(min)>X(j)) then 
                    min=j
                end if   !Se cambian las posiciones para que el minimo se quede a la izquierda
            end do
            aux=X(i)
            X(i)=X(min)  
            X(min)=aux 
            write(unit=10, fmt='(8(I4))', iostat=ierr) X
        end do
    end subroutine ord_seleccion
    
end module