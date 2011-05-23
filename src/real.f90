
Program oooo

  Character (len=1000) :: fname, cd
  Integer, Allocatable :: Nesc(:), Ncuenta(:)
  Character (len=20), Allocatable :: cand(:), Rvot(:)
  Real, Allocatable :: Rper(:)

  Integer :: Nmax, Nleo, Kont

  Type candidatura
     Character (len=20) :: name
     Real :: Rvotos
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
  
  ALLOCATE(cand(kont), Nesc(kont), Rvot(kont), Rper(kont), res(kont))
  Rewind(69)
  Do I = 1, kont
     Read(69,*, END=100, ERR=200)cand(I), Nesc(I), Rvot(I)
     Write(*,*)Rvot(I)
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

     If (Ncand == 0) Then ! Candidatura nueva
        Ndist = Ndist + 1
        Ncand = Ndist
     End If

     res(Ncand)%name   = cand(I)
!     res(Ncand)%Rvotos = res(Ncand)%Rvotos + Rvot(I)*1000.0
     res(Ncand)%Nconc  = res(Ncand)%Nconc + Nesc(I)
  End Do
  

  Write(*,*)Ndist, kont

  NMax = MaxVal(res(:)%Nconc)
  Write(*,*)'# Maximo: ', Nmax
  
  ALLOCATE(Ncuenta(Nmax))
  Ncuenta = 0
  Do I = 1, Ndist
     Ncuenta(res(I)%Nconc) = Ncuenta(res(I)%Nconc) + 1
  End Do

  Do I = 1, Nmax
     If (Ncuenta(I) .ne. 0) Then
        Write(*,*)I, Ncuenta(I)
     End If
  End Do
  


  Stop
End Program oooo
