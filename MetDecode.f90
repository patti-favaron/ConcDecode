! MetDecode - Basic diagnostic and QC procedure for CALMET data files
!
! Copyright 2017 by Servizi Territorio srl
!                   All rights reserved
!
! Written by: Mauri Favaron
!
program MetDecode

	use Calpuff6Files
	use Options
	use Calendar
	
	implicit none
	
	! Locals
	type(CalmetType)		:: tMet
	type(MetDecodeOptsType)	:: tOpts
	character(len=256)		:: sInputFile
	character(len=256)		:: sOutputFile
	integer					:: iRetCode
	integer					:: iSize
	real					:: rMinU, rMinV, rMinW, rMinT
	real					:: rMaxU, rMaxV, rMaxW, rMaxT
	real					:: rAvgU, rAvgV, rAvgW, rAvgT
	real					:: rMinCU, rMinCV, rMinCW, rMinCT
	real					:: rMaxCU, rMaxCV, rMaxCW, rMaxCT
	real					:: rAvgCU, rAvgCV, rAvgCW, rAvgCT
	real					:: rMaxWind2, rMaxWind3, rAvgWoverVel, rMaxWoverVel
	real					:: rAvgWind2, rAvgWind3
	real					:: rAvgCWind2, rAvgCWind3, rMaxCWind2, rMaxCWind3
	real					:: rAvgCWoverVel, rMaxCWoverVel
	real					:: rMinH		! Minimum terrain height (m)
	real					:: rMaxH		! Maximum terrain height (m)
	real					:: rMinZ		! Minimum grid height (m)
	real					:: rMaxZ		! Maximum grid height (m)
	real					:: rAvgWstar
	real					:: rMinWstar
	real					:: rMaxWstar
	real					:: rAvgZi
	real					:: rMinZi
	real					:: rMaxZi
	real					:: rAvgRg
	real					:: rMinRg
	real					:: rMaxRg
	integer, dimension(6)	:: ivStab
	integer					:: iYear, iMonth, iDay, iHour, iMinute, iSecond
	integer					:: iJDay
	integer					:: iTempTime
	integer					:: i
	integer					:: j
	integer					:: k
	integer					:: iX
	integer					:: iY
	real					:: rX
	real					:: rY
	integer					:: iStab
	real					:: rZi
	real					:: rL
	real					:: rUstar
	real					:: rWstar
	real					:: rPrec
	real					:: rGroundTemp
	real					:: rRho
	real					:: rRg
	integer					:: iHrel
	real					:: rLowestHgtU
	real					:: rLowestHgtV
	real					:: rLowestHgtW
	real					:: rLowestHgtT
	real, dimension(:), allocatable		:: rvTpot
	integer, dimension(:), allocatable	:: ivTimeStamp
	integer, dimension(:), allocatable	:: ivUrel
	real, dimension(:), allocatable		:: rvRain
	integer, dimension(:), allocatable	:: ivPrecCode
	real, dimension(:), allocatable		:: rvRg
	real, dimension(:), allocatable		:: rvUstar
	real, dimension(:), allocatable		:: rvL
	real, dimension(:), allocatable		:: rvZi
	integer, dimension(:), allocatable	:: ivIstab
	real, dimension(:,:), allocatable	:: rmU
	real, dimension(:,:), allocatable	:: rmV
	real, dimension(:,:), allocatable	:: rmW
	real, dimension(:,:), allocatable	:: rmTemp
	integer								:: iBaseTime
	integer								:: iCurTime
	
	! Get parameters
	iRetCode = tOpts % forMetDecode()
	
	! Dispatch processing
	if(tOpts % iProcessingType == 1) then	! --check-topo - Check basic topographic and critical options settings
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		close(10)
		
		! Compute minimum and maximum terrain heights, and compare them to
		! minimum and maximum grid heights
		rMinH = minval(tMet % rmElev)
		rMaxH = maxval(tMet % rmElev)
		rMinZ = minval(tMet % rvZfacem)
		rMaxZ = maxval(tMet % rvZfacem)
		
		! Write data (descriptive style)
		open(11, file=tOpts % sOutputFile, status='unknown', action='write')
		write(11, "('Domain size')")
		write(11, "('===========')")
		write(11, "('')")
		write(11, "('P_SW -> x: ',f11.3,'  y: ',f11.3)") &
			tMet % rXorigr, tMet % rYorigr
		write(11, "('P_NE -> x: ',f11.3,'  y: ',f11.3)") &
			tMet % rXorigr + tMet % iNx * tMet % rDgrid, &
			tMet % rYorigr + tMet % iNy * tMet % rDgrid
		write(11, "('')")
		write(11, "('Grid heights (above ground: terrain-following scheme used')")
		write(11, "('=========================================================')")
		write(11, "('')")
		do i = 1, size(tMet % rvZfacem)
			write(11, "('Height idx: ',i4,'  Z=',f7.2)") i, tMet % rvZfacem(i)
		end do
		write(11, "('')")
		write(11, "('Terrain vs Grid heights')")
		write(11, "('=======================')")
		write(11, "('')")
		write(11, &
			"('Min.terrain hgt (m): ',f7.2," // &
			"'   Max.terrain hgt (m): ',f7.2," // &
			"'   Min.grid hgt (m): ',f7.2," // &
			"'   Max.grid hgt (m): ',f7.2)") rMinH, rMaxH, rMinZ, rMaxZ
		write(11, "('Delta grid / Delta terrain: ',f6.2,'%')") 100. * (rMaxZ - rMinZ) / (rMaxH - rMinH)
		write(11, "('')")
		write(11, "('Sf.Stations')")
		write(11, "('===========')")
		write(11, "('')")
		if(tMet % iNssta > 0) then
			do i = 1, tMet % iNssta
				write(11, "('Stn: ',i4,'  P=(',f9.1,',',f9.1,')  NearCells=',i10)") &
					i, tMet % rXorigr + tMet % rvXssta(i), tMet % rYorigr + tMet % rvYssta(i), &
					count(tMet % imNears == i)
			end do
		else
			write(11, "('--None--')")
		end if
		write(11, "('')")
		write(11, "('Ua.Stations')")
		write(11, "('===========')")
		write(11, "('')")
		if(tMet % iNusta > 0) then
			do i = 1, tMet % iNusta
				write(11, "('Stn: ',i4,'  P=(',f9.1,',',f9.1,')')") &
					i, tMet % rXorigr + tMet % rvXusta(i), tMet % rYorigr + tMet % rvYusta(i)
			end do
		else
			write(11, "('--None--')")
		end if
		write(11, "('')")
		write(11, "('Pr.Stations')")
		write(11, "('===========')")
		write(11, "('')")
		if(tMet % iNpsta > 0) then
			do i = 1, tMet % iNpsta
				write(11, "('Stn: ',i4,'  P=(',f9.1,',',f9.1,')')") &
					i, tMet % rXorigr + tMet % rvXpsta(i), tMet % rYorigr + tMet % rvYpsta(i)
			end do
		else
			write(11, "('--None--')")
		end if
		write(11, "('')")
		close(11)
		
	elseif(tOpts % iProcessingType == 2) then	! --check-whole - Check basic wind/temp data consistency
	
		! Process input file
		open(11, file=tOpts % sOutputFile, status='unknown', action='write')
		write(11,"('Date.Time,Second," // &
			"A,B,C,D,E,F," // &
			"Wstar,Wstar.min,Wstar.max,Zi,Zi.min,Zi.max," // &
			"Rg,Rg.min,Rg.max," // &
			"U,V,W,T,U.min,V.min,W.min,T.min,U.max,V.max,W.max," // &
			"T.max,Uc,Vc,Wc,Tc,Uc.min,Vc.min,Wc.min,Tc.min,Uc.max,Vc.max,Wc.max,Tc.max," // &
			"Wind.3,Wind.3.Max,Wind.2,Wind.2.Max," // &
			"W.over.Vel,W.over.Vel.max," // &
			"Wind.3c,Wind.3c.Max,Wind.2c,Wind.2c.Max," // &
			"Wc.over.Velc,Wc.over.Velc.max')")
		
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Get data
		do
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			print *, tMet % iNdathrb, tMet % iIbsec
			ivStab = 0
			do i = 1, tMet % iNx
				do j = 1, tMet % iNy
					k = tMet % imIpgt(i,j)
					ivStab(k) = ivStab(k) + 1
				end do
			end do
			iSize = tMet % iNx * tMet % iNy
			rAvgWstar = sum(tMet % rmWstar) / iSize
			rMinWstar = minval(tMet % rmWstar)
			rMaxWstar = maxval(tMet % rmWstar)
			rAvgZi = sum(tMet % rmZi) / iSize
			rMinZi = minval(tMet % rmZi)
			rMaxZi = maxval(tMet % rmZi)
			rAvgRg = sum(tMet % rmQsw) / iSize
			rMinRg = minval(tMet % rmQsw)
			rMaxRg = maxval(tMet % rmQsw)
			iSize = tMet % iNx * tMet % iNy * tMet % iNz
			rMinU = minval(tMet % raU)
			rMinV = minval(tMet % raV)
			rMinW = minval(tMet % raW)
			rMinT = minval(tMet % raZtemp)
			rMaxU = maxval(tMet % raU)
			rMaxV = maxval(tMet % raV)
			rMaxW = maxval(tMet % raW)
			rMaxT = maxval(tMet % raZtemp)
			rAvgU = sum(tMet % raU) / iSize
			rAvgV = sum(tMet % raV) / iSize
			rAvgW = sum(tMet % raW) / iSize
			rAvgT = sum(tMet % raZtemp) / iSize
			rMaxWind2 = maxval(sqrt(tMet%raU**2 + tMet%raV**2))
			rMaxWind3 = maxval(sqrt(tMet%raU**2 + tMet%raV**2 + tMet%raW**2))
			rAvgWind2 = sum(sqrt(tMet%raU**2 + tMet%raV**2)) / iSize
			rAvgWind3 = sum(sqrt(tMet%raU**2 + tMet%raV**2 + tMet%raW**2)) / iSize
			rMaxWoverVel = &
				maxval( &
					tMet % raW / sqrt(tMet%raU**2 + tMet%raV**2), &
					mask = tMet%raU**2 + tMet%raV**2 > 0.)
			rAvgWoverVel = &
				sum( &
					tMet % raW / sqrt(tMet%raU**2 + tMet%raV**2), &
					mask = tMet%raU**2 + tMet%raV**2 > 0.) / &
				count(tMet%raU**2 + tMet%raV**2 > 0.)
			iSize = (tMet % iNx - 2) * (tMet % iNy - 2) * tMet % iNz
			rMaxCWind2 = maxval(sqrt(tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
				tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2))
			rMaxCWind3 = maxval(sqrt(tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
				tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
				tMet%raW(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2))
			rAvgCWind2 = sum(sqrt(tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
				tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2)) / iSize
			rAvgCWind3 = sum(sqrt(tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
				tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
				tMet%raW(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2)) / iSize
			rMaxCWoverVel = &
				maxval( &
					tMet % raW(2:tMet % iNx-1, 2:tMet % iNy - 1,:) / &
						sqrt(tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
						tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2), &
					mask = tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
						tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 > 0.)
			rAvgCWoverVel = &
				sum( &
					tMet % raW(2:tMet % iNx-1, 2:tMet % iNy - 1,:) / &
						sqrt(tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
							tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2), &
					mask = tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
						tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 > 0.) / &
				count(tMet%raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 + &
					tMet%raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)**2 > 0.)
			rMinCU = minval(tMet % raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rMinCV = minval(tMet % raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rMinCW = minval(tMet % raW(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rMinCT = minval(tMet % raZtemp(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rMaxCU = maxval(tMet % raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rMaxCV = maxval(tMet % raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rMaxCW = maxval(tMet % raW(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rMaxCT = maxval(tMet % raZtemp(2:tMet % iNx-1, 2:tMet % iNy - 1,:))
			rAvgCU = sum(tMet % raU(2:tMet % iNx-1, 2:tMet % iNy - 1,:)) / iSize
			rAvgCV = sum(tMet % raV(2:tMet % iNx-1, 2:tMet % iNy - 1,:)) / iSize
			rAvgCW = sum(tMet % raW(2:tMet % iNx-1, 2:tMet % iNy - 1,:)) / iSize
			rAvgCT = sum(tMet % raZtemp(2:tMet % iNx-1, 2:tMet % iNy - 1,:)) / iSize
			write(11,"(i10,',',i4,6(',',i5),45(',',f7.2))") &
				tMet % iNdathrb, &
				tMet % iIbsec, &
				ivStab, &
				rAvgWstar, rMinWstar, rMaxWstar, &
				rAvgZi, rMinZi, rMaxZi, &
				rAvgRg, rMinRg, rMaxRg, &
				rAvgU, rAvgV, rAvgW, rAvgT, &
				rMinU, rMinV, rMinW, rMinT, &
				rMaxU, rMaxV, rMaxW, rMaxT, &
				rAvgCU, rAvgCV, rAvgCW, rAvgCT, &
				rMinCU, rMinCV, rMinCW, rMinCT, &
				rMaxCU, rMaxCV, rMaxCW, rMaxCT, &
				rAvgWind3, rMaxWind3, rAvgWind2, rMaxWind2, &
				rAvgWoverVel, rMaxWoverVel, &
				rAvgCWind3, rMaxCWind3, rAvgCWind2, rMaxCWind2, &
				rAvgCWoverVel, rMaxCWoverVel
		end do
		
		close(11)
		
	elseif(tOpts % iProcessingType == 3) then	! --check-point - Check local data consistency
	
		! Process input file
		open(11, file=tOpts % sOutputFile, status='unknown', action='write')
		write(11,"('Date.Time,Second," // &
			"Stability,Zi,L,Ustar,Wstar,Prec,Ground.Temp,Rho,Rg,Hrel," // &
			"Lower.U,U.Min,U.Avg,U.Max," // &
			"Lower.V,V.Min,V.Avg,V.Max," // &
			"Lower.W,W.Min,W.Avg,W.Max')" &
		)
		
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Locate index of grid cell center closest to desired point
		if( &
			tOpts % rXp < tMet % rXorigr .or. tOpts % rXp > tMet % rXorigr + tMet % iNx * tMet % rDgrid .or. &
			tOpts % rYp < tMet % rYorigr .or. tOpts % rYp > tMet % rYorigr + tMet % iNy * tMet % rDgrid &
		) then
			print *,'MetDecode:: error: Desired point is outside the meteorological domain'
			stop
		end if
		iX = ceiling((tOpts % rXp - tMet % rXorigr)/tMet % rDgrid)
		iY = ceiling((tOpts % rYp - tMet % rYorigr)/tMet % rDgrid)
		print *,"Closest point has indices ", iX, ",", iY
		
		! Get data
		do
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			print *, tMet % iNdathrb, tMet % iIbsec
			iStab       = tMet % imIpgt(iX,iY)
			rZi         = tMet % rmZi(iX,iY)
			rL          = max(min(tMet % rmEl(iX,iY), 9999.9), -9999.9)
			rUstar      = tMet % rmUstar(iX,iY)
			rWstar      = tMet % rmWstar(iX,iY)
			rPrec       = tMet % rmRmm(iX,iY)
			rGroundTemp = tMet % rmTempk(iX,iY)
			rRho        = tMet % rmRho(iX,iY)
			rRg         = tMet % rmQsw(iX,iY)
			iHrel       = tMet % imIrh(iX,iY)
			rLowestHgtU  = tMet % raU(iX,iY,1)
			rAvgU       = sum(tMet % raU(iX,iY,:)) / tMet % iNz
			rMinU       = minval(tMet % raU(iX,iY,:))
			rMaxU       = maxval(tMet % raU(iX,iY,:))
			rLowestHgtV  = tMet % raV(iX,iY,1)
			rAvgV       = sum(tMet % raV(iX,iY,:)) / tMet % iNz
			rMinV       = minval(tMet % raV(iX,iY,:))
			rMaxV       = maxval(tMet % raV(iX,iY,:))
			rLowestHgtW  = tMet % raW(iX,iY,1)
			rAvgW       = sum(tMet % raW(iX,iY,:)) / tMet % iNz
			rMinW       = minval(tMet % raW(iX,iY,:))
			rMaxW       = maxval(tMet % raW(iX,iY,:))
			write(11,"(i10,',',i4,',',i1,8(',',f8.2),',',i3,12(',',f7.2))") &
				tMet % iNdathrb, &
				tMet % iIbsec, &
				iStab, rZi, rL, rUstar, rWstar, rPrec, rGroundTemp, rRho, rRg, iHrel, &
				rLowestHgtU, rMinU, rAvgU, rMaxU, &
				rLowestHgtV, rMinV, rAvgV, rMaxV, &
				rLowestHgtW, rMinW, rAvgW, rMaxW
		end do
		close(11)
	
	elseif(tOpts % iProcessingType == 4) then	! --check-point - Check local data consistency
	
		! Process input file
		open(11, file=tOpts % sOutputFile, status='unknown', action='write')
		write(11,"('Date.Time,Second," // &
			"Stability,Zi,L,Ustar,Wstar,Prec,Ground.Temp,Rho,Rg,Hrel," // &
			"U," // &
			"V," // &
			"W," // &
			"T," // &
			"Tpot,DTpot.Dz')")
		
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		if(allocated(rvTpot)) deallocate(rvTpot)
		allocate(rvTpot(max(tMet % iNz,3)))
		
		! Locate index of grid cell center closest to desired point
		if( &
			tOpts % rXp < tMet % rXorigr .or. tOpts % rXp > tMet % rXorigr + tMet % iNx * tMet % rDgrid .or. &
			tOpts % rYp < tMet % rYorigr .or. tOpts % rYp > tMet % rYorigr + tMet % iNy * tMet % rDgrid &
		) then
			print *,'MetDecode:: error: Desired point is outside the meteorological domain'
			stop
		end if
		iX = ceiling((tOpts % rXp - tMet % rXorigr)/tMet % rDgrid)
		iY = ceiling((tOpts % rYp - tMet % rYorigr)/tMet % rDgrid)
		print *,"Closest point has indices ", iX, ",", iY
		
		! Get data
		do
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			print *, tMet % iNdathrb, tMet % iIbsec
			iStab       = tMet % imIpgt(iX,iY)
			rZi         = tMet % rmZi(iX,iY)
			rL          = tMet % rmEl(iX,iY)
			rUstar      = tMet % rmUstar(iX,iY)
			rWstar      = tMet % rmWstar(iX,iY)
			rPrec       = tMet % rmRmm(iX,iY)
			rGroundTemp = tMet % rmTempk(iX,iY)
			rRho        = tMet % rmRho(iX,iY)
			rRg         = tMet % rmQsw(iX,iY)
			iHrel       = tMet % imIrh(iX,iY)
			rAvgU       = tMet % raU(iX,iY,tOpts % iZ)
			rAvgV       = tMet % raV(iX,iY,tOpts % iZ)
			rAvgW       = tMet % raW(iX,iY,tOpts % iZ)
			rAvgT       = tMet % raZtemp(iX,iY,tOpts % iZ)
			rvTpot = -9999.9
			rvTpot(1:tMet % iNz) = tMet % raZtemp(iX,iY,:)*(exp(0.000119*tMet % rvZfacem))**0.286
			write(11,"(i10,',',i4,',',i1,8(',',f8.2),',',i3,5(',',f7.2),',',f12.7)") &
				tMet % iNdathrb, &
				tMet % iIbsec, &
				iStab, rZi, rL, rUstar, rWstar, rPrec, rGroundTemp, rRho, rRg, iHrel, &
				rAvgU, &
				rAvgV, &
				rAvgW, &
				rAvgT, &
				rvTpot(tOpts % iZ), &
				(rvTpot(tOpts % iZ + 1)-rvTpot(tOpts % iZ)) / &
					(tMet % rvZfacem(tOpts % iZ + 1) - tMet % rvZfacem(tOpts % iZ))
		end do
		deallocate(rvTpot)
		close(11)
	
	elseif(tOpts % iProcessingType == 5) then	! --get-comment - Get comment section from CALMET file.
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		close(10)
		
		! Write data (descriptive style)
		open(11, file=tOpts % sOutputFile, status='unknown', action='write')
		do i = 1, size(tMet % svComment)
			write(11, "(a)") trim(tMet % svComment(i))
		end do
		close(11)
		
	elseif(tOpts % iProcessingType == 6) then	! --check-site - Check site characteristics
	
		! Process input file
		open(11, file=tOpts % sOutputFile, status='unknown', action='write')
		
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Locate index of grid cell center closest to desired point
		if( &
			tOpts % rXp < tMet % rXorigr .or. tOpts % rXp > tMet % rXorigr + tMet % iNx * tMet % rDgrid .or. &
			tOpts % rYp < tMet % rYorigr .or. tOpts % rYp > tMet % rYorigr + tMet % iNy * tMet % rDgrid &
		) then
			write(11,"('MetDecode:: error: Desired point is outside the meteorological domain')")
			close(11)
			stop
		end if
		iX = ceiling((tOpts % rXp - tMet % rXorigr)/tMet % rDgrid)
		iY = ceiling((tOpts % rYp - tMet % rYorigr)/tMet % rDgrid)
		
		! Get and write data
		write(11, "('Site coordinates: X=',f9.1,',   Y=',f9.1)") tOpts % rXp, tOpts % rYp
		write(11, "('Closest grid point indices: iX=',i4,',   Y=',i4)") iX, iY
		write(11, "('Closest grid point coordinates: X=',f9.1,',   Y=',f9.1)") &
			tMet % rXorigr + tMet % rDgrid * (iX-1) + tMet % rDgrid/2.0, &
			tMet % rYorigr + tMet % rDgrid * (iY-1) + tMet % rDgrid/2.0
		write(11,"('Grid point elevation: H=',f6.1)") tMet % rmElev(iX,iY)
		if(iX > 1 .and. iX < tMet % iNx .and. iY > 1 .and. iY < tMet % iNy) then
			write(11,"('Steepness along X axis: dH/dx=',f9.6)") &
				(((tMet % rmElev(iX+1,iY) - tMet % rmElev(iX,iY))/tMet % rDgrid) + &
				 ((tMet % rmElev(iX,iY) - tMet % rmElev(iX-1,iY))/tMet % rDgrid)) / 2.0
			write(11,"('Steepness along Y axis: dH/dx=',f9.6)") &
				(((tMet % rmElev(iX,iY+1) - tMet % rmElev(iX,iY))/tMet % rDgrid) + &
				 ((tMet % rmElev(iX,iY) - tMet % rmElev(iX,iY-1))/tMet % rDgrid)) / 2.0
		else
			write(11,"('Steepness not computed for cells on border')")
		end if
		write(11,"('Land use category: Lu=',i1)") tMet % imIlandu(iX,iY)
		write(11,"('Aerodynamic roughness length: z0=',f6.1)") tMet % rmZ0(iX,iY)
		write(11,"('Index of closest station: N=',i6)") tMet % imNears(iX,iY)
		write(11,"('Leaf area index: LAI=',f6.1)") tMet % rmXlai(iX,iY)
		close(11)
	
	elseif(tOpts % iProcessingType == 7) then	! --get-ctdm-met - Get point data to CTDM surface+profile files
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Locate index of grid cell center closest to desired point
		if( &
			tOpts % rXp < tMet % rXorigr .or. tOpts % rXp > tMet % rXorigr + tMet % iNx * tMet % rDgrid .or. &
			tOpts % rYp < tMet % rYorigr .or. tOpts % rYp > tMet % rYorigr + tMet % iNy * tMet % rDgrid &
		) then
			print *,'MetDecode:: error: Desired point is outside the meteorological domain'
			stop
		end if
		iX = ceiling((tOpts % rXp - tMet % rXorigr)/tMet % rDgrid)
		iY = ceiling((tOpts % rYp - tMet % rYorigr)/tMet % rDgrid)
		print *,"Closest point has indices ", iX, ",", iY
		
		! Reserve workspace
		allocate( &
			ivTimeStamp(tMet % iIrlg), &
			ivUrel(tMet % iIrlg), &
			rvRain(tMet % iIrlg), &
			ivPrecCode(tMet % iIrlg), &
			rvRg(tMet % iIrlg), &
			rvUstar(tMet % iIrlg), &
			rvL(tMet % iIrlg), &
			rvZi(tMet % iIrlg), &
			ivIstab(tMet % iIrlg), &
			rmU(tMet % iIrlg, SIZE(tMet % rvZfacem)), &
			rmV(tMet % iIrlg, SIZE(tMet % rvZfacem)), &
			rmTemp(tMet % iIrlg, SIZE(tMet % rvZfacem)) &
		)
		
		! Populate workspace with file data
		i = 0
		do
		
			! Get next record
			i = i + 1
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			
			! Get date and time in full form starting from CALMET packed form
			iTempTime = tMet % iNdathrb
			iHour = MOD(iTempTime, 100)
			iTempTime = iTempTime / 100
			iJDay = MOD(iTempTime, 1000)
			iYear = iTempTime / 1000
			call PackTime(iTempTime, iYear, 1, 1, 0, 0, 0)
			ivTimeStamp(i) = iTempTime + (iJDay-1)*86400 + iHour*3600
			
			! Get meteorological and micro-meteorological data
			ivUrel(i)     = tMet % imIrh(iX,iY)
			rvRain(i)     = tMet % rmRmm(iX,iY)
			ivPrecCode(i) = tMet % imIpcode(iX,iY)
			rvRg(i)       = tMet % rmQsw(iX,iY)
			rvUstar(i)    = tMet % rmUstar(iX,iY)
			rvL(i)        = tMet % rmEl(iX,iY)
			rvZi(i)       = tMet % rmZi(iX,iY)
			ivIstab(i)    = tMet % imIpgt(iX,iY)
			do j = 1, tMet % iNz
				rmU(i,j)      = tMet % raU(iX,iY,j)
				rmV(i,j)      = tMet % raV(iX,iY,j)
				rmTemp(i,j)   = tMet % raZtemp(iX,iY,j)
			end do
			
			! Inform user
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			print "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", iYear, iMonth, iDay, iHour, iMinute, iSecond
			
		end do
		close(10)
		
		! Save data
		iRetCode = WriteDataToCalpuffCtdm( &
			11, 12, &
			tOpts % sSurfaceFile, &
			tOpts % sProfileFile, &
			tMet % sPmap, tMet % sAxtz, &
			tMet % iDeltaTime, &
			tMet % rmZ0(iX, iY), &
			tMet % rvZfacem(2), &		! Typically first nonzero height
			tMet % rvZfacem, &
			ivTimeStamp, &
			ivUrel, &
			rvRain, ivPrecCode, &
			rvRg, &
			rvUstar, &
			rvL, &
			rvZi, &
			ivIstab, &
			rmU, rmV, rmTemp &
		)
	
		! Leave
		deallocate( &
			ivTimeStamp, &
			ivUrel, &
			rvRain, &
			ivPrecCode, &
			rvRg, &
			rvUstar, &
			rvL, &
			rvZi, &
			ivIstab, &
			rmU, &
			rmV, &
			rmTemp &
		)
		
	elseif(tOpts % iProcessingType == 8) then	! --get-profile - Get point data to CTDM surface+profile files
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Locate index of grid cell center closest to desired point
		if( &
			tOpts % rXp < tMet % rXorigr .or. tOpts % rXp > tMet % rXorigr + tMet % iNx * tMet % rDgrid .or. &
			tOpts % rYp < tMet % rYorigr .or. tOpts % rYp > tMet % rYorigr + tMet % iNy * tMet % rDgrid &
		) then
			print *,'MetDecode:: error: Desired point is outside the meteorological domain'
			stop
		end if
		iX = ceiling((tOpts % rXp - tMet % rXorigr)/tMet % rDgrid)
		iY = ceiling((tOpts % rYp - tMet % rYorigr)/tMet % rDgrid)
		print *,"Closest point has indices ", iX, ",", iY
		
		! Reserve workspace
		allocate( &
			ivTimeStamp(tMet % iIrlg), &
			ivUrel(tMet % iIrlg), &
			rvRain(tMet % iIrlg), &
			ivPrecCode(tMet % iIrlg), &
			rvRg(tMet % iIrlg), &
			rvUstar(tMet % iIrlg), &
			rvL(tMet % iIrlg), &
			rvZi(tMet % iIrlg), &
			ivIstab(tMet % iIrlg), &
			rmU(tMet % iIrlg, SIZE(tMet % rvZfacem)), &
			rmV(tMet % iIrlg, SIZE(tMet % rvZfacem)), &
			rmW(tMet % iIrlg, SIZE(tMet % rvZfacem)), &
			rmTemp(tMet % iIrlg, SIZE(tMet % rvZfacem)) &
		)
		
		! Populate workspace with file data
		i = 0
		do
		
			! Get next record
			i = i + 1
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			
			! Get date and time in full form starting from CALMET packed form
			iTempTime = tMet % iNdathrb
			iHour = MOD(iTempTime, 100)
			iTempTime = iTempTime / 100
			iJDay = MOD(iTempTime, 1000)
			iYear = iTempTime / 1000
			call PackTime(iTempTime, iYear, 1, 1, 0, 0, 0)
			ivTimeStamp(i) = iTempTime + (iJDay-1)*86400 + iHour*3600
			
			! Get meteorological and micro-meteorological data
			ivUrel(i)     = tMet % imIrh(iX,iY)
			rvRain(i)     = tMet % rmRmm(iX,iY)
			ivPrecCode(i) = tMet % imIpcode(iX,iY)
			rvRg(i)       = tMet % rmQsw(iX,iY)
			rvUstar(i)    = tMet % rmUstar(iX,iY)
			rvL(i)        = tMet % rmEl(iX,iY)
			rvZi(i)       = tMet % rmZi(iX,iY)
			ivIstab(i)    = tMet % imIpgt(iX,iY)
			do j = 1, tMet % iNz
				rmU(i,j)      = tMet % raU(iX,iY,j)
				rmV(i,j)      = tMet % raV(iX,iY,j)
				rmW(i,j)      = tMet % raW(iX,iY,j)
				rmTemp(i,j)   = tMet % raZtemp(iX,iY,j)
			end do
			
			! Inform user
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			print "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", iYear, iMonth, iDay, iHour, iMinute, iSecond
			
		end do
		close(10)
		
		! Save data
		iRetCode = WriteDataToStandardProfile( &
			11, &
			tOpts % sProfileFile, &
			tMet % sPmap, tMet % sAxtz, &
			tMet % iDeltaTime, &
			tMet % rmZ0(iX, iY), &
			tMet % rvZfacem(2), &		! Typically first nonzero height
			tMet % rvZfacem, &
			ivTimeStamp, &
			ivUrel, &
			rvRain, ivPrecCode, &
			rvRg, &
			rvUstar, &
			rvL, &
			rvZi, &
			ivIstab, &
			rmU, rmV, rmW, rmTemp &
		)
	
		! Leave
		deallocate( &
			ivTimeStamp, &
			ivUrel, &
			rvRain, &
			ivPrecCode, &
			rvRg, &
			rvUstar, &
			rvL, &
			rvZi, &
			ivIstab, &
			rmU, &
			rmV, &
			rmW, &
			rmTemp &
		)
		
	elseif(tOpts % iProcessingType == 9) then	! --convert-grads - Convert CALMET data file to GrADS, binary form
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Write CTL and open data file for output
		iRetCode = tMet % openGrADS(11, 12, tOpts % sGrADSFile, tOpts % sGrADSCtl)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Error opening GrADS file or composing CTL - Return code = ', iRetCode
			stop
		end if
		
		i = 0
		do
		
			! Get next record
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			
			! Write current record
			iRetCode = tMet % writeGrADS(12)
			if(iRetCode /= 0) then
				print *, "Error writing data"
				exit
			end if
			
			! Inform user
			i = i + 1
			print *,"Step ", i, " of ", tMet % iIrlg
			
		end do
		iRetCode = tMet % closeGrADS(12)
		close(10)
		
	elseif(tOpts % iProcessingType == 10) then	! --get-snaps - Extract 2D wind field snaps at given level from CALMET data file, binary form
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Check level number makes sense on current run
		if(tOpts % iZ < 1 .or. tOpts % iZ > tMet % iNz) then
			print *,'error:: MetDecode: Level required, ', tOpts % iZ, ', is not in range 1..', tMet % iNz
			stop
		end if
		
		! Write header
		print "('Date, Max.Vel')"
		
		! Compute simulation base time
		call PackTime(iBaseTime, &
			tMet % iIbyr, tMet % iIbmo, tMet % iIbdy, &
			tMet % iIbhr, tMet % iIbsec/60, mod(tMet % iIbsec, 60) &
		)
		
		i = 0
		do
		
			! Get next record
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			
			! Generate next file
			write(sOutputFile, "(a,i5.5,'.csv')") trim(tOpts % sOutputPrefix), i
			open(11, file=sOutputFile, status='unknown', action='write')
			write(11,"('X,Y,U,V')")
			
			! Write current record
			do iX = 1, tMet % iNx
				rX = tMet % rXorigr + 0.5 * tMet % rDgrid + tMet % rDgrid * (iX - 1)
				do iY = 1, tMet % iNy
					rY = tMet % rYorigr + 0.5 * tMet % rDgrid + tMet % rDgrid * (iY - 1)
					write(11, "(2(f11.3,','),f7.3,',',f7.3)") &
						rX, rY, &
						tMet % raU(iX, iY, tOpts % iZ), &
						tMet % raV(iX, iY, tOpts % iZ)
				end do
			end do
			close(11)
			
			! Inform user
			i = i + 1
			iCurTime = iBaseTime + (i - 1) * tMet % iDeltaTime
			call UnpackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			print "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',f7.3)", &
				iYear, iMonth, iDay, iHour, iMinute, iSecond, &
				maxval(sqrt(tMet % raU(:, :, tOpts % iZ)**2 + tMet % raV(:, :, tOpts % iZ)**2))
			
		end do
		iRetCode = tMet % closeGrADS(12)
		close(10)
		
	elseif(tOpts % iProcessingType == 11) then	! --get-caline-met - Get point data to Caline files
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Locate index of grid cell center closest to desired point
		if( &
			tOpts % rXp < tMet % rXorigr .or. tOpts % rXp > tMet % rXorigr + tMet % iNx * tMet % rDgrid .or. &
			tOpts % rYp < tMet % rYorigr .or. tOpts % rYp > tMet % rYorigr + tMet % iNy * tMet % rDgrid &
		) then
			print *,'MetDecode:: error: Desired point is outside the meteorological domain'
			stop
		end if
		iX = ceiling((tOpts % rXp - tMet % rXorigr)/tMet % rDgrid)
		iY = ceiling((tOpts % rYp - tMet % rYorigr)/tMet % rDgrid)
		print *,"Closest point has indices ", iX, ",", iY
		
		! Reserve workspace
		allocate( &
			ivTimeStamp(tMet % iIrlg), &
			rvL(tMet % iIrlg), &
			rvZi(tMet % iIrlg), &
			ivIstab(tMet % iIrlg), &
			rmU(tMet % iIrlg, SIZE(tMet % rvZfacem)), &
			rmV(tMet % iIrlg, SIZE(tMet % rvZfacem)) &
		)
		
		! Populate workspace with file data
		i = 0
		do
		
			! Get next record
			i = i + 1
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			
			! Get date and time in full form starting from CALMET packed form
			iTempTime = tMet % iNdathrb
			iHour = MOD(iTempTime, 100)
			iTempTime = iTempTime / 100
			iJDay = MOD(iTempTime, 1000)
			iYear = iTempTime / 1000
			call PackTime(iTempTime, iYear, 1, 1, 0, 0, 0)
			ivTimeStamp(i) = iTempTime + (iJDay-1)*86400 + iHour*3600
			
			! Get meteorological and micro-meteorological data
			rvL(i)        = tMet % rmEl(iX,iY)
			rvZi(i)       = tMet % rmZi(iX,iY)
			ivIstab(i)    = tMet % imIpgt(iX,iY)
			do j = 1, tMet % iNz
				rmU(i,j)      = tMet % raU(iX,iY,j)
				rmV(i,j)      = tMet % raV(iX,iY,j)
			end do
			
			! Inform user
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			print "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", iYear, iMonth, iDay, iHour, iMinute, iSecond
			
		end do
		close(10)
		
		! Save data
		iRetCode = WriteDataToCaline( &
			11, &
			tOpts % sSurfaceFile, &
			tMet % iDeltaTime, &
			tMet % rmZ0(iX, iY), &
			tMet % rvZfacem(2), &		! Typically first nonzero height
			ivTimeStamp, &
			rvL, &
			rvZi, &
			ivIstab, &
			rmU, rmV &
		)
	
		! Leave
		deallocate( &
			ivTimeStamp, &
			rvL, &
			rvZi, &
			ivIstab, &
			rmU, &
			rmV &
		)
		
	else ! Dump minimal data report to screen
	
		! Get header
		iRetCode = tMet % getHeader(10, tOpts % sCalmetFile)
		if(iRetCode /= 0) then
			print *,'error:: MetDecode: Ill-shaped input file - Return code = ', iRetCode
			stop
		end if
		
		! Get data
		do
			iRetCode = tMet % getRecord(10)
			if(iRetCode /= 0) exit
			print *, tMet % iNdathrb, tMet % iIbsec, maxval(tMet % raU), maxval(tMet % raV), maxval(tMet % raW)
		end do
		
		close(10)
		
	end if

end program MetDecode
