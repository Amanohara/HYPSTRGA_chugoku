      character com1*1, com16*16,com79*79,com_J*1,com_E*1
      character com_U*1,com_I*1,out_fl*24
!
      com_J='J'
      com_E='E'
      com_U='U'
      com_I='I'
!
      open( 10, file='input_fl.dat')
!
    1 continue
      read(10,1000, end=900) com1,com16,com79
 1000 format(a1,a16,a79)
      if(com1 .eq. com_J) go to 3
      if(com1 .eq. com_U) go to 3
      if(com1 .eq. com_I) go to 3
      go to 900
    3 continue
      write(out_fl, 1200) com1,com16
 1200 format('out_fl/',a1,a16)
      open(11,file=out_fl)
      write(11,1100) com1,com16,com79
 1100 format(a1,a16,a79)
    2 continue
      read(10,1000, end=900) com1,com16,com79
      if(com1 .eq. com_E) then
      write(11, 1102) com1
 1102 format(a1)
         close(11)
        go to 1
      endif
      write(11,1100) com1,com16,com79
      go to 2
  900 continue
      close(11)
      stop
      end
