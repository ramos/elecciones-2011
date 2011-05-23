
Program oooo

  Character (len=1000) :: fname, candidatura
  Integer, Allocatable :: Nesc(:)
  
  Integer :: Nmax, Nleo, Kont

  Read(*,'(01A)')fname
  Open(Unit=69, File=Trim(fname), ACTION="READ")
  
  Nmax = -1
  Kont = 0
  Do While (.True.)
     kont = kont + 1
     Read(69,*, END=10, ERR=20)candidatura, Nleo
     Nmax = Max(Nmax, Nleo)
20   Continue
  End Do
10 Continue
  
  Write(*,*)'# Maximo: ', Nmax
  
  Rewind(69)
  ALLOCATE(Nesc(Nmax))
  Nesc = 0
  Do I = 1, kont
     Read(69,*, END=100, ERR=200)candidatura, Nleo
     Nesc(Nleo) = Nesc(Nleo) + 1
200  Continue
  End Do
100 Continue

  Do I = 1, Nmax
     If (Nesc(I) > 0) Then
        Write(*,*)I, Nesc(I)
     End If
  End Do


  Stop
End Program oooo
