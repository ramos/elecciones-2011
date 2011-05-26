
Program oooo

  USE NonNumeric

  Character (len=1000) :: fname, cd
  Integer, Allocatable :: Nesc(:), Ncuenta(:), Nvot(:), Ipt(:), Nord(:)
  Character (len=20), Allocatable :: cand(:)
  Real, Allocatable :: Rper(:)

  Integer :: Nmax, Nleo, Kont

  Type candidatura
     Character (len=20) :: name
     Integer :: Nvotos
     Integer :: Nconc
  End type candidatura

  Type (candidatura), Allocatable :: res(:)



  Read(*,'(01A)')fname
  Open(Unit=69, File=Trim(fname), ACTION="READ")
  
  Nmax = -1
  Kont = 0
  Do While (.True.)
     kont = kont + 1
     Read(69,*, END=10, ERR=20)cd, Nleo
     Nmax = Max(Nmax, Nleo)
!     Write(*,*)'#'//Trim(cd)//'#'
20   Continue
  End Do
10 Continue
  
  ALLOCATE(cand(kont), Nesc(kont), Nvot(kont), Rper(kont), res(kont))
  Rewind(69)
  Do I = 1, kont
     Read(69,*, END=100, ERR=200)cand(I), Nesc(I), Nvot(I)
!     Write(*,*)Nvot(I)
200  Continue
  End Do
100 Continue
  close(69)

  Ndist = 0
  Do I = 1, kont
     Ncand = 0
     Do J = 1, I-1
        If (cand(I) == res(J)%name) Then
!           Write(*,*)'Aqui: ', Trim(cand(I)), Trim(res(J)%Name)
           Ncand = J
        End If
     End Do
     Ncand = 0

     If (Ncand == 0) Then ! Candidatura nueva
        Ndist = Ndist + 1
        Ncand = Ndist
     End If

     res(Ncand)%name   = cand(I)
     res(Ncand)%Nvotos = res(Ncand)%Nvotos + Nvot(I)
     res(Ncand)%Nconc  = res(Ncand)%Nconc + Nesc(I)
  End Do
  

  Write(*,*)'#', Ndist, kont

  NMax = MaxVal(res(:)%Nconc)
  Write(*,*)'# Maximo: ', Nmax, MaxVal(res(:)%Nvotos)
  
  Allocate(Ipt(Ndist), Nord(Ndist))
  Nord(1:Ndist) = res(1:Ndist)%Nvotos
  CALL Qsort(Nord, Ipt)

  Open (unit=10, File='rep.dat')
  Open (unit=11, File='norep.dat')

  Do I = Ndist, 1, -1
     If (res(Ipt(I))%Nconc == 0) Then
        Write(11,*)"'"//Trim(res(Ipt(I))%name)//"'", res(Ipt(I))&
             &%nvotos, res(Ipt(I))%Nconc, Ndist-I+1
     Else
        Write(10,*)"'"//Trim(res(Ipt(I))%name)//"'", res(Ipt(I))%nvotos, res(Ipt(I))%Nconc, Ndist-I+1
     End If
  End Do
  close(10)
  Close(11)


  MM = Sum(res(:)%nvotos, res(:)%nconc < 1)
  Write(*,*)'# Total de votos:                              ', Sum(res(:)%nvotos)
  Write(*,*)'# Votos a partidos con menos de 10 concejales: ', MM



  Stop
End Program oooo
