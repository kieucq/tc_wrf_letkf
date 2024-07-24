      subroutine compute_err_z(serr_u,serr_v,serr_t,serr_q,serr_p,   &
                               k,nz,err_u,err_v,err_t,err_q,err_p)
      integer k,nz
      real serr_u,serr_v,serr_t,serr_q,serr_p
      real err_u,err_v,err_t,err_q,err_p
      real qscale,pscale
      qscale = 1.
      pscale = 2. 
      err_u  = serr_u*(1.+float(k-1)/float(nz))
      err_v  = err_u
      err_t  = serr_t*(1.+float(k-1)/float(nz))
      err_q  = serr_q*exp(-qscale*(k-1)**2./float(nz)**2.)
      err_p  = serr_p*exp(pscale*(k-1)/float(nz))
      return
      end
