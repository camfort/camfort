PROGRAM example2
   IMPLICIT NONE
   INTEGER Highpw , Ibmcdc , Idate(3) , Idrum , Ihalf , Intp , Iprec , Jhalf , Kshift , Ldict , Linkno , Lnknos(15) , Lowpw , Lpch ,&
         & Lqro , Mach , Machx , Mask2 , Mask3 , Mtisa , Mxfl , Mzero , Nbpc , Nbpw , Ncpw , Nlpp , Nudflw , Nwpic , Outtap ,       &
         & Sperlk , Sysbuf , Two(32)
   CHARACTER*7 Machos
   CHARACTER*11 Mchnam
   COMMON /chmach/ Mchnam , Machos
   COMMON /lhpwx / Lowpw , Highpw , Nwpic , Nudflw , Mxfl , Kshift , Mtisa
   COMMON /machin/ Machx , Ihalf , Jhalf , Lqro
   COMMON /sem   / Mask2 , Mask3 , Lnknos
   COMMON /two   / Two , Mzero
   INTEGER abcd , ak , i , imnth , iyr1 , iyr2 , ka , locf , m1(110) , m2(110) , mask , mconst(220) , mvax , nmach , order , qp ,   &
         & recl
   INTEGER andf , complf , khrfn1 , locfx , lshift , rshift
   EXTERNAL andf , complf , lshift , rshift
   EQUIVALENCE (m1(1),mconst(1)) , (m2(1),mconst(111))
   DATA nmach/22/ , m1/200 , 4100 , 871 , 1042 , 1028 , 1028 , 1028 , 1028 , 1028 , 1028 , 1028 , 2052 , 1028 , 2052 , 2052 , 1028 ,&
      & 1028 , 1028 , 1028 , 1028 , 1028 , 1028 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 ,     &
      & 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 5000 , 5518 , 5518 , 4208 , 5518 , 5518 , 5518 , 5518 , 5518 , 5518 , &
      & 5518 , 5509 , 5518 , 5500 , 5500 , 5500 , 5500 , 5500 , 5500 , 5500 , 5518 , 550 , 636 , 832 , 936 , 660 , 832 , 832 , 832 ,&
      & 832 , 832 , 832 , 832 , 864 , 832 , 864 , 864 , 832 , 832 , 832 , 832 , 832 , 832 , 832 , 200 , 240 , 210 , 110 , 210 ,     &
      & 210 , 240 , 240 , 240 , 210 , 200 , 180 , 240 , 100 , 100 , 200 , 200 , 200 , 200 , 200 , 210 , 200/
   DATA m2/703 , 707 , 103 , 707 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 ,  &
      & 104 , 104 , 38 , 75 , 38 , 321 , 38 , 38 , 38 , 38 , 38 , 38 , 38 , 2465 , 38 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 38 , 0 , 1650 , &
      & 1650 , 1849 , 1475 , 875 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 ,&
      & 1675 , 975 , 1675 , 1 , 4096 , 4096 , 262144 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 0 , 0 , 0 ,   &
      & 0 , 0 , 0 , 0 , 4096 , 0 , 000 , 2426 , 2760 , 4896 , 2355 , 2355 , 2352 , 2355 , 2355 , 2355 , 2355 , 4896 , 2352 , 4896 , &
      & 4896 , 2355 , 2355 , 2355 , 000 , 000 , 2355 , 000/
   Mach = 7
   Sysbuf = mconst(Mach)
   Ibmcdc = 1
   IF ( Mach==2 .OR. Mach==4 ) Ibmcdc = 0
   i = Mach + nmach
   Intp = mconst(i)/100
   PRINT *, Intp
END PROGRAM
