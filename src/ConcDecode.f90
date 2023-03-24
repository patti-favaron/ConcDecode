! ConcDecode - Convert from CALPUFF current output file to "01" (ISC style) format.
!
! =============================================================================
!
! MIT License
!
! Copyright (c) 2023 Patrizia Favaron
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! Major changes:
! ==============
!
! Version 1.00 by Patrizia "Patti" Favaron (circa 2007)
!
! 14. 07. 2017		The previous versions all assumed an unformatted
!					CON file, produced with a compiler of which we had only
!					an example output, and from which a decoding was
!					devised by reverse engineering data.
!
!					Now, 2017, time has finally come for Fortran developers
!					to standardize the UNformATTED form.
!
!					Code has been (drastically) simplified as a consequence.
!
!					I've started from "ConcDecode_final.f90", as improved
!					by Alberto (v 1.05).
!
!					(Patti)   (The 14th of July!! Usually I paid coffee to
!					all friends and colleagues, but today I forgot to do.
!					This might be anyway a nice 14th July present!)
!
! 24. 07. 2017		Improvement made by introducing a second diagnostic
!					file containing a useful time series of concentration indicators.
!
!					(Patti, Lorella)
!
! 06. 12. 2017		Interface changed from the old, rigid style to a more
!					flexible, option-based one, modeled after MetDecode.
!
!					(Patti)
!
! 30. 12. 2017		Added new option, to save data for movie building.
!
!					(Patti)
!
! 14. 01. 2018      Various corrections made.
!
!					(Patti)
!
! 15. 01. 2018      Support added for Calpuff 7 concentration files (entirely different from version 6).
!
!					(Patti)
!
! 03. 02. 2018		Debug and fixes made on quantile calculation part.
!
!					(Patti)
!
! 22. 02. 2019		Many additions and corrections.
!
!					(Patti)
!
! 07. 10. 2019		Important addition: CO-specific processing.
!
!					(Patti)
!
! 11. 01. 2020		Another important addition: randomized delta check
!
!					(Patti)

program ConcDecode

	use calendar
	use Options
	use calpuff6files

	use pbl_met

    implicit none

    ! Locals
    integer								:: iRetCode
    type(CalpuffType)					:: tCalp
    type(CalpuffType)					:: tCalp2
	type(ConcDecodeOptsType)			:: tOpts
	integer								:: iSeries, jSeries
	integer								:: iSeriesSup, jSeriesSup
    real, dimension(4)                  :: rvWeight
	integer								:: iNumFrame
	integer								:: iCpuffMode
	character(len=128)					:: sErrStr
	real, dimension(:), allocatable		:: rvConc
	real, dimension(:,:), allocatable	:: rmQuantile
	real, dimension(:,:,:), allocatable	:: raConc
	real(8), dimension(:), allocatable	:: rvTimeStamp
	real, dimension(:,:,:), allocatable	:: raConcMaxSub
	real, dimension(:,:,:), allocatable	:: raMaxMvAvg8
	real(8), dimension(:), allocatable	:: rvTimeStampSub
	integer, dimension(:), allocatable	:: ivTimeBlock
	integer, dimension(:), allocatable	:: ivBlockBegin
	integer, dimension(:), allocatable	:: ivBlockEnd
	integer, dimension(:), allocatable	:: ivTimeStamp
	integer, dimension(:), allocatable	:: ivDay
	integer, dimension(:), allocatable	:: ivHour
	integer								:: iBlock
	integer								:: iHoursPerBlock
	integer								:: iNumBlocks
	integer								:: iNumRecords
	integer								:: iNx
	integer								:: iNy
	type(DateTime)						:: tDateTime
	character(len=23)					:: sTimeStamp
	integer								:: i
	integer								:: j
	integer								:: iX
	integer								:: iY
	real								:: rDelta
	real								:: rX
	real								:: rY
	integer								:: iYear
	integer								:: iJulDay
	integer								:: iHour
	integer(8)							:: iTimeStamp
	logical								:: lCompatible
	real								:: rMean1
	real								:: rMean2
	real								:: rLMean1
	real								:: rLMean2
	real								:: rFB
	real								:: rNAD
	real								:: rNMSE
	real								:: rMSE
	real								:: rMAE
	integer								:: iNumZeroNonzeroA
	integer								:: iNumZeroNonzeroB
	integer								:: k, l
	real								:: rAccum
	integer								:: iNumBothNonZero
	integer								:: iNumWithin
	integer								:: iNumWithin2
	real								:: rGM
	real								:: rGV
	integer, dimension(:), allocatable	:: ivSeries
	integer, dimension(:), allocatable	:: jvSeries
	integer, dimension(:), allocatable	:: ivSeriesSup
	integer, dimension(:), allocatable	:: jvSeriesSup
	real, dimension(:,:), allocatable	:: rmWeight
	integer, dimension(:), allocatable	:: ivTotalIndex
	integer, dimension(:), allocatable	:: ivPartialIndexX
	integer, dimension(:), allocatable	:: ivPartialIndexY
	integer, dimension(:), allocatable	:: ivSampleIndex
	integer, dimension(:), allocatable	:: ivSampleX
	integer, dimension(:), allocatable	:: ivSampleY
	real, dimension(:,:), allocatable	:: rmSampleConcA
	real, dimension(:,:), allocatable	:: rmSampleConcB
	real								:: rFAC2
	real								:: rFAC2_2
	real, dimension(:), allocatable		:: rvFB
	real, dimension(:), allocatable		:: rvNMSE
	real, dimension(:), allocatable		:: rvGM
	real, dimension(:), allocatable		:: rvGV
	real, dimension(:), allocatable		:: rvFAC2
	real, dimension(:), allocatable		:: rvFAC2_2
	real, dimension(:), allocatable		:: rvNAD
	logical								:: lDifferingNumSteps
	logical								:: lDifferingGrids
	character(len=256)					:: sCompFileName

    ! Get parameters
    iRetCode = tOpts % forConcDecode()

    ! Normalize Calpuff file name, and decide whether it is version 6 or 7
    if(tOpts % sCalpuffFile(1:1) == '@') then
		iCpuffMode = 7
		tOpts % sCalpuffFile = tOpts % sCalpuffFile(2:)
		print *, 'Starting Calpuff 7 file processing'
	else
		iCpuffMode = 6	! Normal mode
		print *, 'Starting Calpuff 6 file processing'
	end if

    ! Dispatch processing
    select case(tOpts % iProcessingType)
    case(1)	! --get-metadata

		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))
		close(10)

		iRetCode = tCalp % writeHeader(10, tOpts)

    case(2)	! --get-comment

		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))
		close(10)

		iRetCode = tCalp % writeComment(10, tOpts)
		if(iRetCode /= 0) call ErrorExit('Problem writing to output file', iRetCode=iRetCode)

    case(3)	! --get-01

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Check some of the options make sense
		if(tOpts % iSpeciesIdx < 1 .or. tOpts % iSpeciesIdx > size(tCalp % svSpeciesName)) then
			call ErrorExit('Index of chemical species not in 1..number of species')
		end if

		! Iterate over all file contents, and write it to 01 file
		iRetCode = tCalp % open01(11, tOpts % sOutputFile)
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % write01(11, tOpts)
			if(iRetCode /= 0) call ErrorExit('Error writing 01 file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(4)	! --get-grd

		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))
		close(10)

		iRetCode = tCalp % writeGridSpec(10, tOpts % sOutputFile)
		if(iRetCode /= 0) call ErrorExit('Problem writing to output file', iRetCode=iRetCode)

    case(5)	! --get-timeseries

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openSeries(11, tOpts, iSeries, jSeries, iSeriesSup, jSeriesSup, rvWeight)
		if(iRetCode /= 0) call ErrorExit('Output series not opened; check coordinates and file name')
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeSeries(11, tOpts, iSeries, jSeries, iSeriesSup, jSeriesSup, rvWeight)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(6)	! --get-receptors

		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))
		close(10)

		iRetCode = tCalp % writeGrid(10, tOpts % sOutputFile)
		if(iRetCode /= 0) call ErrorExit('Problem writing to output file', iRetCode=iRetCode)

    case(7)	! --get-summaries

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openSummary(100, tOpts % sOutputFile)
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeSummary(100, tOpts)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(10)

    case(8)	! --get-maxima

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openMaxima(11, tOpts % sOutputFile)
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeMaxima(11, tOpts)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(10)

    case(9)	! --get-exceedances

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % resetExceeds()
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % updateExceeds(tOpts)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(10)

		! Write counts
		iRetCode = tCalp % writeExceeds(10, tOpts)

    case(10)	! --get-quantile

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
		end do
		close(10)

		! Get quantile
		allocate(rvConc(iNumRecords), rmQuantile(tCalp % iNxSampling, tCalp % iNySampling))
		do i = 1, size(raConc, dim=2)
			do j = 1, size(raConc, dim=3)
				rvConc = raConc(:,i,j)
				rmQuantile(i,j) = Quantile(rvConc, tOpts % rQuantile, QUANT_POPULATION)
			end do
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			print *, "Processing row ", iY, " out of ", tCalp % iNySampling
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					rmQuantile(iX,iY) * tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvConc, rmQuantile)
		deallocate(raConc)

    case(12)	! --get-movie-data

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iNumFrame = 0
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeConc(11, tOpts, iNumFrame)
			if(iRetCode /= 0) call ErrorExit('Error writing movie frame', iRetCode=iRetCode)
			iNumFrame = iNumFrame + 1
		end do
		close(10)

    case(13)	! --get-mean

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
		end do
		close(10)
		iRetCode = tCalp % writeMean(11, tOpts)
		if(iRetCode /= 0) call ErrorExit('Error writing mean field', iRetCode=iRetCode)

    case(14)	! --get-max

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
		end do
		close(10)
		iRetCode = tCalp % writeMax(11, tOpts)
		if(iRetCode /= 0) call ErrorExit('Error writing max field', iRetCode=iRetCode)

    case(15)	! --get-mean-max-8

		iHoursPerBlock = 8

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = maxval(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					sum(raConcMaxSub(:,iX,iY), dim=1) / size(raConcMaxSub(:,iX,iY), dim=1) &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(16)	! --get-max-mean-8

		iHoursPerBlock = 8

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = sum(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1) / &
											(ivBlockEnd(iBlock) - ivBlockBegin(iBlock) + 1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					maxval(raConcMaxSub(:,iX,iY), dim=1) * tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(17)	! --get-mean-max-24

		iHoursPerBlock = 24

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = maxval(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					sum(raConcMaxSub(:,iX,iY), dim=1) / size(raConcMaxSub(:,iX,iY), dim=1) &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(18)	! --get-max-mean-24

		iHoursPerBlock = 24

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = sum(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1) / &
											(ivBlockEnd(iBlock) - ivBlockBegin(iBlock) + 1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					maxval(raConcMaxSub(:,iX,iY), dim=1) * tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(19)	! --get-quantile-mean-24

		iHoursPerBlock = 24

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = sum(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1) / &
											(ivBlockEnd(iBlock) - ivBlockBegin(iBlock) + 1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					Quantile(raConcMaxSub(:,iX,iY), tOpts % rQuantile, QUANT_POPULATION) &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(20)	! --get-quantile-mean-8

		iHoursPerBlock = 8

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = sum(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1) / &
											(ivBlockEnd(iBlock) - ivBlockBegin(iBlock) + 1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					Quantile(raConcMaxSub(:,iX,iY), tOpts % rQuantile, QUANT_POPULATION) &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

	case(25)	! --field-compare

		! Access files
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in primary Calpuff file', trim(sErrStr))
		iRetCode = tCalp2 % getHeader(11, tOpts % sOtherCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in second Calpuff file', trim(sErrStr))

		! Verify the two data files are compatible
		lDifferingNumSteps = tCalp % iModelSteps == tCalp2 % iModelSteps
		lDifferingGrids    = &
			tCalp % iMinSampX == tCalp2 % iMinSampX .and. &
			tCalp % iMaxSampX == tCalp2 % iMaxSampX .and. &
			tCalp % iMinSampY == tCalp2 % iMinSampY .and. &
			tCalp % iMaxSampY == tCalp2 % iMaxSampY .and. &
			tCalp % iMinCompX == tCalp2 % iMinCompX .and. &
			tCalp % iMaxCompX == tCalp2 % iMaxCompX .and. &
			tCalp % iMinCompY == tCalp2 % iMinCompY .and. &
			tCalp % iMaxCompY == tCalp2 % iMaxCompY .and. &
			tCalp % iMeshFactor == tCalp2 % iMeshFactor .and. &
			tCalp % iNx == tCalp2 % iNx .and. &
			tCalp % iNy == tCalp2 % iNy .and. &
			tCalp % rX0 == tCalp2 % rX0 .and. &
			tCalp % rY0 == tCalp2 % rY0 .and. &
			tCalp % rDx == tCalp2 % rDx .and. &
			tCalp % rDy == tCalp2 % rDy
		lCompatible = lDifferingNumSteps .and. lDifferingGrids
		if(.not.lCompatible) then
			print *, "The two Calpuff files contain incompatible data:"
			if(lDifferingNumSteps) print *, "- Number of steps differ: ", tCalp % iModelSteps, "  vs  ", tCalp2 % iModelSteps
			if(lDifferingGrids)    print *, "- Grids differ."
			call ErrorExit("Terminating execution", "")
		end if

		! Collect array sizing data
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(rvTimeStamp(iNumRecords))

		! Iterate over all file contents, and perform comparisons
		open(12, file=tOpts % sOutputFile, status='unknown', action='write')
		write(12, "('Time.Stamp, Mean.First, Mean.Second, " // &
			"FB, NMSE, GM, GV, FAC2, FAC2_2, NAD, MSE, MAE, N.Zero.First.Nonzero.Second, N.Nonzero.First.Zero.Second')")
		do i = 1, iNumRecords

			! Gather concntration fields
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp2 % getRecord(11, tOpts)
			if(iRetCode /= 0) exit
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0

			! Compare concentration fields
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			sTimeStamp = tDateTime % toISO()
			rMSE = sum((tCalp % rmConc(:,:,tOpts % iSpeciesIdx) - tCalp2 % rmConc(:,:,tOpts % iSpeciesIdx))**2) / &
					(size(tCalp % rmConc, dim=1) * size(tCalp % rmConc, dim=2))
			rMAE = sum(abs(tCalp % rmConc(:,:,tOpts % iSpeciesIdx) - tCalp2 % rmConc(:,:,tOpts % iSpeciesIdx))) / &
					(size(tCalp % rmConc, dim=1) * size(tCalp % rmConc, dim=2))
			rMean1 = sum(tCalp % rmConc(:,:,tOpts % iSpeciesIdx))  / (size(tCalp % rmConc, dim=1) * size(tCalp % rmConc, dim=2))
			rMean2 = sum(tCalp2 % rmConc(:,:,tOpts % iSpeciesIdx)) / (size(tCalp2 % rmConc, dim=1) * size(tCalp % rmConc, dim=2))
			rFB    = 2. * sum(tCalp % rmConc(:,:,tOpts % iSpeciesIdx) - tCalp2 % rmConc(:,:,tOpts % iSpeciesIdx)) / &
			            sum(tCalp % rmConc(:,:,tOpts % iSpeciesIdx) + tCalp2 % rmConc(:,:,tOpts % iSpeciesIdx))
			rNMSE  = rMSE / (rMean1 * rMean2)
			rLMean1 = sum(log(tCalp % rmConc(:,:,tOpts % iSpeciesIdx)), mask=(tCalp % rmConc(:,:,tOpts % iSpeciesIdx) > 0.))
			rLMean2 = sum(log(tCalp2 % rmConc(:,:,tOpts % iSpeciesIdx)), mask=(tCalp2 % rmConc(:,:,tOpts % iSpeciesIdx) > 0.))
			rGM     = exp(rLMean1 - rLMean2)
			iNumBothNonZero = 0
			rAccum = 0.
			do iX = 1, size(tCalp % rmConc, dim=1)
				do iy = 1, size(tCalp % rmConc, dim=2)
					if(tCalp % rmConc(iX, iY, tOpts % iSpeciesIdx) > 0. .and. tCalp2 % rmConc(iX, iY, tOpts % iSpeciesIdx) > 0.) then
						iNumBothNonZero = iNumBothNonZero + 1
						rAccum = rAccum + (log(tCalp % rmConc(iX, iY, tOpts % iSpeciesIdx)) - &
							log(tCalp2 % rmConc(iX, iY, tOpts % iSpeciesIdx)))**2
					end if
				end do
			end do
			rGV     = exp(rAccum/iNumBothNonZero)
			iNumWithin  = 0
			iNumWithin2 = 0
			do iX = 1, size(tCalp % rmConc, dim=1)
				do iy = 1, size(tCalp % rmConc, dim=2)
					if( &
						0.5 * tCalp % rmConc(iX, iY, tOpts % iSpeciesIdx) <= tCalp2 % rmConc(iX, iY, tOpts % iSpeciesIdx) .and. &
						tCalp2 % rmConc(iX, iY, tOpts % iSpeciesIdx) <= 2. * tCalp % rmConc(iX, iY, tOpts % iSpeciesIdx) &
					) then
						iNumWithin = iNumWithin + 1
					end if
					if( &
						0.5 * tCalp2 % rmConc(iX, iY, tOpts % iSpeciesIdx) <= tCalp % rmConc(iX, iY, tOpts % iSpeciesIdx) .and. &
						tCalp % rmConc(iX, iY, tOpts % iSpeciesIdx) <= 2. * tCalp2 % rmConc(iX, iY, tOpts % iSpeciesIdx) &
					) then
						iNumWithin2 = iNumWithin2 + 1
					end if
				end do
			end do
			rFAC2 = real(iNumWithin) / (size(tCalp % rmConc, dim=1) * size(tCalp % rmConc, dim=2))
			rFAC2_2 = real(iNumWithin2) / (size(tCalp % rmConc, dim=1) * size(tCalp % rmConc, dim=2))
			rNAD  = rMAE / (rMean1 + rMean2)
			iNumZeroNonzeroA = 0
			iNumZeroNonzeroB = 0
			do k = 1, iNx
				do l = 1, iNy
					if(tCalp % rmConc(k,l,tOpts % iSpeciesIdx) <= 1.e-9 .and. tCalp2 % rmConc(k,l,tOpts % iSpeciesIdx) > 1.e-9) then
						iNumZeroNonzeroA = iNumZeroNonzeroA + 1
					end if
					if(tCalp % rmConc(k,l,tOpts % iSpeciesIdx) > 1.e-9 .and. tCalp2 % rmConc(k,l,tOpts % iSpeciesIdx) <= 1.e-9) then
						iNumZeroNonzeroB = iNumZeroNonzeroB + 1
					end if
				end do
			end do

			! Write comparison indicators
			write(12, "(a23,11(',',e15.5),2(',',i10))") &
				sTimeStamp, &
				rMean1, rMean2, &
				rFB, rNMSE, rGM, rGV, rFAC2, rFAC2_2, rNAD, &
				rMSE, rMAE, iNumZeroNonzeroA, iNumZeroNonzeroB

			end do

		case(23)	! --get-quantile-moving-average-8

			iHoursPerBlock = 8

			! Access file
			iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
			if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

			! Collect array sizing data, and try reserving workspace
			iNumRecords = tCalp % iModelSteps
			iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
			iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
			allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
			if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

			! Iterate over all file contents, and write it to series file
			do i = 1, iNumRecords
				iRetCode = tCalp % getRecord(10, tOpts)
				if(iRetCode /= 0) exit
				raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
				iYear   = tCalp % iYear
				iJulDay = tCalp % iJulDay
				iHour   = tCalp % iHour
				tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
				rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
				iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
				iTimeStamp = floor(rvTimeStamp(i))
				ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
			end do
			close(10)

			! Process blocks
			allocate(ivTimeStamp(size(rvTimeStamp)), ivDay(size(rvTimeStamp)), ivHour(size(rvTimeStamp)), stat=iRetCode)
			if(iRetCode /= 0) call ErrorExit('Workspace not allocated')
			ivTimeStamp = int(rvTimeStamp, kind=4)
			ivDay       = (ivTimeStamp - minval(ivTimeStamp)) / 86400 + 1
			ivHour      = (ivTimeStamp - (ivDay - 1)/86400) / 3600
			iRetCode = co_m8(ivDay, ivHour, raConc, raMaxMvAvg8, .true.)
			if(iRetCode /= 0) call ErrorExit('Error computing 8-hours moving averages')

			! Write current species values
			open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
			if(iRetCode /= 0) call ErrorExit("Output file not opened")
			write(11, "('X, Y, Conc')")
			rDelta = tCalp % rDx / tCalp % iMeshFactor
			do iY = 1, tCalp % iNySampling
				rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
				do iX = 1, tCalp % iNySampling
					rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
					write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
						Quantile(raMaxMvAvg8(:,iX,iY), tOpts % rQuantile, QUANT_POPULATION) &
							* tOpts % rFactorConversion * tOpts % rScale
				end do
			end do
			close(11)
			deallocate(ivTimeStamp, ivDay, ivHour)

			! Free resources
			deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
			deallocate(raConc, rvTimeStamp, ivTimeBlock)

		case(21)	! --get-max-moving-average-8

			iHoursPerBlock = 8

			! Access file
			iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
			if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

			! Collect array sizing data, and try reserving workspace
			iNumRecords = tCalp % iModelSteps
			iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
			iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
			allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
			if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

			! Iterate over all file contents, and write it to series file
			do i = 1, iNumRecords
				iRetCode = tCalp % getRecord(10, tOpts)
				if(iRetCode /= 0) exit
				raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
				iYear   = tCalp % iYear
				iJulDay = tCalp % iJulDay
				iHour   = tCalp % iHour
				tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
				rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
				iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
				iTimeStamp = floor(rvTimeStamp(i))
				ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
			end do
			close(10)

			! Process blocks
			allocate(ivTimeStamp(size(rvTimeStamp)), ivDay(size(rvTimeStamp)), ivHour(size(rvTimeStamp)), stat=iRetCode)
			if(iRetCode /= 0) call ErrorExit('Workspace not allocated')
			ivTimeStamp = int(rvTimeStamp, kind=4)
			ivDay       = (ivTimeStamp - minval(ivTimeStamp)) / 86400 + 1
			ivHour      = (ivTimeStamp - (ivDay - 1)/86400) / 3600
			iRetCode = co_m8(ivDay, ivHour, raConc, raMaxMvAvg8, .true.)
			if(iRetCode /= 0) call ErrorExit('Error computing 8-hours moving averages')

			! Write current species values
			open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
			if(iRetCode /= 0) call ErrorExit("Output file not opened")
			write(11, "('X, Y, Conc')")
			rDelta = tCalp % rDx / tCalp % iMeshFactor
			do iY = 1, tCalp % iNySampling
				rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
				do iX = 1, tCalp % iNySampling
					rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
					write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
						maxval(raMaxMvAvg8(:,iX,iY)) &
							* tOpts % rFactorConversion * tOpts % rScale
				end do
			end do
			close(11)
			deallocate(ivTimeStamp, ivDay, ivHour)

			! Free resources
			deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
			deallocate(raConc, rvTimeStamp, ivTimeBlock)

		case(26)	! --get-mean-arm2

			! Access file
			iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
			if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

			! Iterate over all file contents, and write it to series file
			do
				iRetCode = tCalp % getRecord(10, tOpts, .true.)
				if(iRetCode /= 0) exit
			end do
			close(10)
			iRetCode = tCalp % writeMean(11, tOpts)
			if(iRetCode /= 0) call ErrorExit('Error writing mean field', iRetCode=iRetCode)

		close(12)
		close(11)
		close(10)

    case(27)	! --get-max-arm2

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		do
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
		end do
		close(10)
		iRetCode = tCalp % writeMax(11, tOpts)
		if(iRetCode /= 0) call ErrorExit('Error writing max field', iRetCode=iRetCode)

		! Free resources
		deallocate(rvTimeStamp)

    case(28)	! --get-max-mean-8-arm2

		iHoursPerBlock = 8

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = sum(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1) / &
											(ivBlockEnd(iBlock) - ivBlockBegin(iBlock) + 1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					maxval(raConcMaxSub(:,iX,iY), dim=1) * tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(29)	! --get-mean-max-8-arm2

		iHoursPerBlock = 8

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = maxval(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					sum(raConcMaxSub(:,iX,iY), dim=1) / size(raConcMaxSub(:,iX,iY), dim=1) &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(30)	! --get-max-mean-24-arm2

		iHoursPerBlock = 24

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = sum(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1) / &
											(ivBlockEnd(iBlock) - ivBlockBegin(iBlock) + 1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					maxval(raConcMaxSub(:,iX,iY), dim=1) * tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(31)	! --get-mean-max-24-arm2

		iHoursPerBlock = 24

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), rvTimeStamp(iNumRecords), ivTimeBlock(iNumRecords), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			iTimeStamp = floor(rvTimeStamp(i))
			ivTimeBlock(i) = iTimeStamp / (iHoursPerBlock*3600)
		end do
		close(10)

		! Convert block number to block index
		ivTimeBlock = ivTimeBlock - minval(ivTimeBlock) + 1

		! Process blocks
		iNumBlocks = maxval(ivTimeBlock)
		allocate(rvTimeStampSub(iNumBlocks), ivBlockBegin(iNumBlocks), ivBlockEnd(iNumBlocks), raConcMaxSub(iNumBlocks, iNx, iNy))
		iBlock = 1
		ivBlockBegin(iBlock) = 1
		do i = 2, size(ivTimeBlock)
			if(ivTimeBlock(i) /= ivTimeBlock(i-1)) then
				ivBlockEnd(iBlock) = i-1
				iBlock = iBlock + 1
				ivBlockBegin(iBlock) = i
			end if
		end do
		ivBlockEnd(iNumBlocks) = size(ivTimeBlock)
		do iBlock = 1, iNumBlocks
			raConcMaxSub(iBlock,:,:) = maxval(raConc(ivBlockBegin(iBlock):ivBlockEnd(iBlock),:,:), dim=1)
			rvTimeStampSub(iBlock)   = rvTimeStamp(ivBlockBegin(iBlock))
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					sum(raConcMaxSub(:,iX,iY), dim=1) / size(raConcMaxSub(:,iX,iY), dim=1) &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvTimeStampSub, ivBlockBegin, ivBlockEnd, raConcMaxSub)
		deallocate(raConc, rvTimeStamp, ivTimeBlock)

    case(32)	! --get-timeseries-arm2

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openSeries(11, tOpts, iSeries, jSeries, iSeriesSup, jSeriesSup, rvWeight)
		if(iRetCode /= 0) call ErrorExit('Output series not opened; check coordinates and file name')
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeSeries(11, tOpts, iSeries, jSeries, iSeriesSup, jSeriesSup, rvWeight, .true.)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(33)	! --get-01-arm2

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Check some of the options make sense
		if(tOpts % iSpeciesIdx < 1 .or. tOpts % iSpeciesIdx > size(tCalp % svSpeciesName)) then
			call ErrorExit('Index of chemical species not in 1..number of species')
		end if

		! Iterate over all file contents, and write it to 01 file
		iRetCode = tCalp % open01(11, tOpts % sOutputFile)
		do
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % write01(11, tOpts)
			if(iRetCode /= 0) call ErrorExit('Error writing 01 file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(34)	! --get-exceedances-arm2

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % resetExceeds()
		do
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % updateExceeds(tOpts)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(10)

		! Write counts
		iRetCode = tCalp % writeExceeds(10, tOpts)

    case(35)	! --get-quantile-arm2

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Collect array sizing data, and try reserving workspace
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(raConc(iNumRecords, iNx, iNy), stat=iRetCode)
		if(iRetCode /= 0) call ErrorExit('Workspace not allocated')

		! Iterate over all file contents, and write it to series file
		do i = 1, iNumRecords
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			raConc(i,:,:) = tCalp % rmConc(:,:,tOpts % iSpeciesIdx)
		end do
		close(10)

		! Get quantile
		allocate(rvConc(iNumRecords), rmQuantile(tCalp % iNxSampling, tCalp % iNySampling))
		do i = 1, size(raConc, dim=2)
			do j = 1, size(raConc, dim=3)
				rvConc = raConc(:,i,j)
				rmQuantile(i,j) = Quantile(rvConc, tOpts % rQuantile, QUANT_POPULATION)
			end do
		end do

		! Write current species values
		open(11, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iRetCode)
		if(iRetCode /= 0) call ErrorExit("Output file not opened")
		write(11, "('X, Y, Conc')")
		rDelta = tCalp % rDx / tCalp % iMeshFactor
		do iY = 1, tCalp % iNySampling
			rY = tCalp % rY0 + (tCalp % iMinSampY - 1) * tCalp % rDy + (iY-1)*rDelta + tCalp % rDy / 2.0
			print *, "Processing row ", iY, " out of ", tCalp % iNySampling
			do iX = 1, tCalp % iNySampling
				rX = tCalp % rX0 + (tCalp % iMinSampX - 1) * tCalp % rDx + (iX-1)*rDelta + tCalp % rDx / 2.0
				write(11, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					rmQuantile(iX,iY) * tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(11)

		! Free resources
		deallocate(rvConc, rmQuantile)
		deallocate(raConc)

    case(36)	! --get-multiseries-interp

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openMultiSeries(11, tOpts, ivSeries, jvSeries, ivSeriesSup, jvSeriesSup, rmWeight)
		if(iRetCode /= 0) call ErrorExit('Output series not opened; check coordinates and file name')
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeMultiSeries( &
				11, &
				tOpts, &
				ivSeries, jvSeries, ivSeriesSup, jvSeriesSup, rmWeight, &
				tOpts % iSpeciesIdx &
			)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(37)	! --get-multiseries-interp-arm2

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openMultiSeries(11, tOpts, ivSeries, jvSeries, ivSeriesSup, jvSeriesSup, rmWeight)
		if(iRetCode /= 0) call ErrorExit('Output series not opened; check coordinates and file name')
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeMultiSeries( &
				11, &
				tOpts, &
				ivSeries, jvSeries, ivSeriesSup, jvSeriesSup, rmWeight, &
				tOpts % iSpeciesIdx, &
				.true. &
			)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(38)	! --get-multiseries-closest

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openMultiSeriesClosest(11, tOpts, ivSeries, jvSeries)
		if(iRetCode /= 0) call ErrorExit('Output series not opened; check coordinates and file name')
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeMultiSeriesClosest( &
				11, &
				tOpts, &
				ivSeries, jvSeries, &
				tOpts % iSpeciesIdx &
			)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(39)	! --get-multiseries-closest-arm2

		! Access file
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in Calpuff file', trim(sErrStr))

		! Iterate over all file contents, and write it to series file
		iRetCode = tCalp % openMultiSeriesClosest(11, tOpts, ivSeries, jvSeries)
		if(iRetCode /= 0) call ErrorExit('Output series not opened; check coordinates and file name')
		do
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp % writeMultiSeriesClosest( &
				11, &
				tOpts, &
				ivSeries, jvSeries, &
				tOpts % iSpeciesIdx, &
				.true. &
			)
			if(iRetCode /= 0) call ErrorExit('Error writing time series file', iRetCode=iRetCode)
		end do
		close(11)
		close(10)

    case(40)	! --compare-randomized

		! Access files
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in primary Calpuff file', trim(sErrStr))
		iRetCode = tCalp2 % getHeader(11, tOpts % sOtherCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in second Calpuff file', trim(sErrStr))

		! Verify the two data files are compatible
		lCompatible = &
			tCalp % iModelSteps == tCalp2 % iModelSteps .and. &
			tCalp % iMinSampX == tCalp2 % iMinSampX .and. &
			tCalp % iMaxSampX == tCalp2 % iMaxSampX .and. &
			tCalp % iMinSampY == tCalp2 % iMinSampY .and. &
			tCalp % iMaxSampY == tCalp2 % iMaxSampY .and. &
			tCalp % iMinCompX == tCalp2 % iMinCompX .and. &
			tCalp % iMaxCompX == tCalp2 % iMaxCompX .and. &
			tCalp % iMinCompY == tCalp2 % iMinCompY .and. &
			tCalp % iMaxCompY == tCalp2 % iMaxCompY .and. &
			tCalp % iMeshFactor == tCalp2 % iMeshFactor .and. &
			tCalp % iNx == tCalp2 % iNx .and. &
			tCalp % iNy == tCalp2 % iNy .and. &
			tCalp % rX0 == tCalp2 % rX0 .and. &
			tCalp % rY0 == tCalp2 % rY0 .and. &
			tCalp % rDx == tCalp2 % rDx .and. &
			tCalp % rDy == tCalp2 % rDy
		if(.not.lCompatible) call ErrorExit("The two Calpuff files contain incompatible data", trim(sErrStr))

		! Collect array sizing data
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(rvTimeStamp(iNumRecords))
		
		! Extract a random sample of indices
		allocate(ivTotalIndex(iNx*iNy), ivPartialIndexX(iNx*iNy), ivPartialIndexY(iNx*iNy))
		allocate(ivSampleIndex(tOpts % iSampleSize), ivSampleX(tOpts % iSampleSize), ivSampleY(tOpts % iSampleSize))
		k = 0
		do i = 1, iNx
			do j = 1, iNy
				k = k + 1
				ivTotalIndex(k)    = k
				ivPartialIndexX(k) = i
				ivPartialIndexY(k) = j
			end do
		end do
		call ransam(ivTotalIndex, ivSampleIndex)
		ivSampleX = ivPartialIndexX(ivSampleIndex)
		ivSampleY = ivPartialIndexY(ivSampleIndex)

		! Iterate over all file contents, and get comparison data
		allocate(rmSampleConcA(iNumRecords, tOpts % iSampleSize), rmSampleConcB(iNumRecords, tOpts % iSampleSize))
		do i = 1, iNumRecords
		
			! Gather concntration fields
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp2 % getRecord(11, tOpts)
			if(iRetCode /= 0) exit
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			
			! Store results
			do j = 1, tOpts % iSampleSize
				rmSampleConcA(i, j) = tCalp % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
				rmSampleConcB(i, j) = tCalp2 % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
			end do
			
		end do
		
		! Reserve workspace for final results
		allocate( &
			rvFB(tOpts % iSampleSize), &
			rvNMSE(tOpts % iSampleSize), &
			rvGM(tOpts % iSampleSize), &
			rvGV(tOpts % iSampleSize), &
			rvFAC2(tOpts % iSampleSize), &
			rvFAC2_2(tOpts % iSampleSize), &
			rvNAD(tOpts % iSampleSize) &
		)

		! Compute results
		do i = 1, tOpts % iSampleSize

			! Compute standard compare parameters
			rMean1     = sum(rmSampleConcA(:,i)) / iNumRecords
			rMean2     = sum(rmSampleConcB(:,i)) / iNumRecords
			rvFB(i)    = 2. * sum(rmSampleConcA(:,i) - rmSampleConcB(:,i)) / sum(rmSampleConcA(:,i) + rmSampleConcB(:,i))
			rMAE       = sum((rmSampleConcA(:,i) - rmSampleConcB(:,i))**2)
			rvNMSE(i)  = rMAE / (rMean1 * rMean2)
			rLMean1    = sum(log(rmSampleConcA(:,i)), mask=(rmSampleConcA(:,i) > 0.))
			rLMean2    = sum(log(rmSampleConcB(:,i)), mask=(rmSampleConcB(:,i) > 0.))
			rvGM(i)    = exp(rLMean1 - rLMean2)
			iNumBothNonZero = 0
			rAccum = 0.
			do j = 1, iNumRecords
				if(rmSampleConcA(j,i) > 0. .and. rmSampleConcB(j,i) > 0.) then
					iNumBothNonZero = iNumBothNonZero + 1
					rAccum = rAccum + (log(rmSampleConcA(j,i)) - log(rmSampleConcB(j,i)))**2
				end if
			end do
			rvGV(i)     = exp(rAccum/iNumBothNonZero)
			iNumWithin  = 0
			iNumWithin2 = 0
			do j = 1, iNumRecords
				if(0.5 * rmSampleConcA(j,i) <= rmSampleConcB(j,i) .and. rmSampleConcB(j,i) <= 2. * rmSampleConcA(j,i)) then
					iNumWithin = iNumWithin + 1
				end if
				if(0.5 * rmSampleConcB(j,i) <= rmSampleConcA(j,i) .and. rmSampleConcA(j,i) <= 2. * rmSampleConcB(j,i)) then
					iNumWithin2 = iNumWithin2 + 1
				end if
			end do
			rvFAC2(i)   = real(iNumWithin)  / iNumRecords
			rvFAC2_2(i) = real(iNumWithin2) / iNumRecords
			rvNAD(i)    = rMAE / (rMean1 + rMean2)
			
		end do
		close(12)

    case(41)	! --compare-randomized-arm2

		! Access files
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in primary Calpuff file', trim(sErrStr))
		iRetCode = tCalp2 % getHeader(11, tOpts % sOtherCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in second Calpuff file', trim(sErrStr))

		! Verify the two data files are compatible
		lCompatible = &
			tCalp % iModelSteps == tCalp2 % iModelSteps .and. &
			tCalp % iMinSampX == tCalp2 % iMinSampX .and. &
			tCalp % iMaxSampX == tCalp2 % iMaxSampX .and. &
			tCalp % iMinSampY == tCalp2 % iMinSampY .and. &
			tCalp % iMaxSampY == tCalp2 % iMaxSampY .and. &
			tCalp % iMinCompX == tCalp2 % iMinCompX .and. &
			tCalp % iMaxCompX == tCalp2 % iMaxCompX .and. &
			tCalp % iMinCompY == tCalp2 % iMinCompY .and. &
			tCalp % iMaxCompY == tCalp2 % iMaxCompY .and. &
			tCalp % iMeshFactor == tCalp2 % iMeshFactor .and. &
			tCalp % iNx == tCalp2 % iNx .and. &
			tCalp % iNy == tCalp2 % iNy .and. &
			tCalp % rX0 == tCalp2 % rX0 .and. &
			tCalp % rY0 == tCalp2 % rY0 .and. &
			tCalp % rDx == tCalp2 % rDx .and. &
			tCalp % rDy == tCalp2 % rDy
		if(.not.lCompatible) call ErrorExit("The two Calpuff files contain incompatible data", trim(sErrStr))

		! Collect array sizing data
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(rvTimeStamp(iNumRecords))
		
		! Extract a random sample of indices
		allocate(ivTotalIndex(iNx*iNy), ivPartialIndexX(iNx*iNy), ivPartialIndexY(iNx*iNy))
		allocate(ivSampleIndex(tOpts % iSampleSize), ivSampleX(tOpts % iSampleSize), ivSampleY(tOpts % iSampleSize))
		k = 0
		do i = 1, iNx
			do j = 1, iNy
				k = k + 1
				ivTotalIndex(k)    = k
				ivPartialIndexX(k) = i
				ivPartialIndexY(k) = j
			end do
		end do
		call ransam(ivTotalIndex, ivSampleIndex)
		ivSampleX = ivPartialIndexX(ivSampleIndex)
		ivSampleY = ivPartialIndexY(ivSampleIndex)

		! Iterate over all file contents, and get comparison data
		allocate(rmSampleConcA(iNumRecords, tOpts % iSampleSize), rmSampleConcB(iNumRecords, tOpts % iSampleSize))
		do i = 1, iNumRecords
		
			! Gather concntration fields
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			iRetCode = tCalp2 % getRecord(11, tOpts, .true.)
			if(iRetCode /= 0) exit
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			
			! Store results
			do j = 1, tOpts % iSampleSize
				rmSampleConcA(i, j) = tCalp % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
				rmSampleConcB(i, j) = tCalp2 % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
			end do
			
		end do
		
		! Reserve workspace for final results
		allocate( &
			rvFB(tOpts % iSampleSize), &
			rvNMSE(tOpts % iSampleSize), &
			rvGM(tOpts % iSampleSize), &
			rvGV(tOpts % iSampleSize), &
			rvFAC2(tOpts % iSampleSize), &
			rvFAC2_2(tOpts % iSampleSize), &
			rvNAD(tOpts % iSampleSize) &
		)

		! Compute results
		do i = 1, tOpts % iSampleSize

			! Compute standard compare parameters
			rMean1     = sum(rmSampleConcA(:,i)) / iNumRecords
			rMean2     = sum(rmSampleConcB(:,i)) / iNumRecords
			rvFB(i)    = 2. * sum(rmSampleConcA(:,i) - rmSampleConcB(:,i)) / sum(rmSampleConcA(:,i) + rmSampleConcB(:,i))
			rMAE       = sum((rmSampleConcA(:,i) - rmSampleConcB(:,i))**2)
			rvNMSE(i)  = rMAE / (rMean1 * rMean2)
			rLMean1    = sum(log(rmSampleConcA(:,i)), mask=(rmSampleConcA(:,i) > 0.))
			rLMean2    = sum(log(rmSampleConcB(:,i)), mask=(rmSampleConcB(:,i) > 0.))
			rvGM(i)    = exp(rLMean1 - rLMean2)
			iNumBothNonZero = 0
			rAccum = 0.
			do j = 1, iNumRecords
				if(rmSampleConcA(j,i) > 0. .and. rmSampleConcB(j,i) > 0.) then
					iNumBothNonZero = iNumBothNonZero + 1
					rAccum = rAccum + (log(rmSampleConcA(j,i)) - log(rmSampleConcB(j,i)))**2
				end if
			end do
			rvGV(i)     = exp(rAccum/iNumBothNonZero)
			iNumWithin  = 0
			iNumWithin2 = 0
			do j = 1, iNumRecords
				if(0.5 * rmSampleConcA(j,i) <= rmSampleConcB(j,i) .and. rmSampleConcB(j,i) <= 2. * rmSampleConcA(j,i)) then
					iNumWithin = iNumWithin + 1
				end if
				if(0.5 * rmSampleConcB(j,i) <= rmSampleConcA(j,i) .and. rmSampleConcA(j,i) <= 2. * rmSampleConcB(j,i)) then
					iNumWithin2 = iNumWithin2 + 1
				end if
			end do
			rvFAC2(i)   = real(iNumWithin)  / iNumRecords
			rvFAC2_2(i) = real(iNumWithin2) / iNumRecords
			rvNAD(i)    = rMAE / (rMean1 + rMean2)

		end do

		! Write results
		open(12, file=tOpts % sOutputFile, status='unknown', action='write')
		write(12, "('iX, iY, FB, NMSE, GM, GV, FAC2, FAC2.2, NAD')")
		do i = 1, tOpts % iSampleSize
			write(12, "(i5, ',', i5, 7(',', e15.7))") &
				ivSampleX(i), &
				ivSampleY(i), &
				rvFB(i), &
				rvNMSE(i), &
				rvGM(i), &
				rvGV(i), &
				rvFAC2(i), &
				rvFAC2_2(i), &
				rvNAD(i)
		end do
		close(12)
		
		! Reclaim workspace for the next users...
		deallocate(rvFB, rvNMSE, rvGM, rvGV, rvFAC2, rvFAC2_2, rvNAD)
		
    case(42)	! --get-multiseries-random-comparison

		! Access files
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in primary Calpuff file', trim(sErrStr))
		iRetCode = tCalp2 % getHeader(11, tOpts % sOtherCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in second Calpuff file', trim(sErrStr))

		! Verify the two data files are compatible
		lCompatible = &
			tCalp % iModelSteps == tCalp2 % iModelSteps .and. &
			tCalp % iMinSampX == tCalp2 % iMinSampX .and. &
			tCalp % iMaxSampX == tCalp2 % iMaxSampX .and. &
			tCalp % iMinSampY == tCalp2 % iMinSampY .and. &
			tCalp % iMaxSampY == tCalp2 % iMaxSampY .and. &
			tCalp % iMinCompX == tCalp2 % iMinCompX .and. &
			tCalp % iMaxCompX == tCalp2 % iMaxCompX .and. &
			tCalp % iMinCompY == tCalp2 % iMinCompY .and. &
			tCalp % iMaxCompY == tCalp2 % iMaxCompY .and. &
			tCalp % iMeshFactor == tCalp2 % iMeshFactor .and. &
			tCalp % iNx == tCalp2 % iNx .and. &
			tCalp % iNy == tCalp2 % iNy .and. &
			tCalp % rX0 == tCalp2 % rX0 .and. &
			tCalp % rY0 == tCalp2 % rY0 .and. &
			tCalp % rDx == tCalp2 % rDx .and. &
			tCalp % rDy == tCalp2 % rDy
		if(.not.lCompatible) call ErrorExit("The two Calpuff files contain incompatible data", trim(sErrStr))

		! Collect array sizing data
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(rvTimeStamp(iNumRecords))
		
		! Extract a random sample of indices
		allocate(ivTotalIndex(iNx*iNy), ivPartialIndexX(iNx*iNy), ivPartialIndexY(iNx*iNy))
		allocate(ivSampleIndex(tOpts % iSampleSize), ivSampleX(tOpts % iSampleSize), ivSampleY(tOpts % iSampleSize))
		k = 0
		do i = 1, iNx
			do j = 1, iNy
				k = k + 1
				ivTotalIndex(k)    = k
				ivPartialIndexX(k) = i
				ivPartialIndexY(k) = j
			end do
		end do
		call ransam(ivTotalIndex, ivSampleIndex)
		ivSampleX = ivPartialIndexX(ivSampleIndex)
		ivSampleY = ivPartialIndexY(ivSampleIndex)

		! Iterate over all file contents, and get comparison data
		allocate(rmSampleConcA(iNumRecords, tOpts % iSampleSize), rmSampleConcB(iNumRecords, tOpts % iSampleSize))
		do i = 1, iNumRecords
		
			! Gather concntration fields
			iRetCode = tCalp % getRecord(10, tOpts)
			if(iRetCode /= 0) exit
			iRetCode = tCalp2 % getRecord(11, tOpts)
			if(iRetCode /= 0) exit
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			
			! Store results
			do j = 1, tOpts % iSampleSize
				rmSampleConcA(i, j) = tCalp % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
				rmSampleConcB(i, j) = tCalp2 % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
			end do
			
		end do
		
		! Print results
		do j = 1, tOpts % iSampleSize

			! Generate actual file name based on prefix and positional indices,
			! then write data to it
			write(sCompFileName, "(a, 2('_', i4.4), '.csv')") trim(tOpts % sOutputFile), ivSampleX(j), ivSampleY(j)
			open(13, file=sCompFileName, status='unknown', action='write')
			write(13, "('Time.Stamp, First.File.Value, Second.File.Value')")
			do i = 1, iNumRecords
				write(13, "(i6, 2(',', e15.7))") i, rmSampleConcA(i, j), rmSampleConcB(i, j)
			end do
			close(13)
			
		end do
		close(12)

    case(43)	! --get-multiseries-random-comp-arm2

		! Access files
		iRetCode = tCalp % getHeader(10, tOpts % sCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in primary Calpuff file', trim(sErrStr))
		iRetCode = tCalp2 % getHeader(11, tOpts % sOtherCalpuffFile, iCpuffMode, sErrStr)
		if(iRetCode /= 0) call ErrorExit('Invalid header in second Calpuff file', trim(sErrStr))

		! Verify the two data files are compatible
		lCompatible = &
			tCalp % iModelSteps == tCalp2 % iModelSteps .and. &
			tCalp % iMinSampX == tCalp2 % iMinSampX .and. &
			tCalp % iMaxSampX == tCalp2 % iMaxSampX .and. &
			tCalp % iMinSampY == tCalp2 % iMinSampY .and. &
			tCalp % iMaxSampY == tCalp2 % iMaxSampY .and. &
			tCalp % iMinCompX == tCalp2 % iMinCompX .and. &
			tCalp % iMaxCompX == tCalp2 % iMaxCompX .and. &
			tCalp % iMinCompY == tCalp2 % iMinCompY .and. &
			tCalp % iMaxCompY == tCalp2 % iMaxCompY .and. &
			tCalp % iMeshFactor == tCalp2 % iMeshFactor .and. &
			tCalp % iNx == tCalp2 % iNx .and. &
			tCalp % iNy == tCalp2 % iNy .and. &
			tCalp % rX0 == tCalp2 % rX0 .and. &
			tCalp % rY0 == tCalp2 % rY0 .and. &
			tCalp % rDx == tCalp2 % rDx .and. &
			tCalp % rDy == tCalp2 % rDy
		if(.not.lCompatible) call ErrorExit("The two Calpuff files contain incompatible data", trim(sErrStr))

		! Collect array sizing data
		iNumRecords = tCalp % iModelSteps
		iNx         = (tCalp % iMaxSampX - tCalp % iMinSampX) * tCalp % iMeshFactor + 1
		iNy         = (tCalp % iMaxSampY - tCalp % iMinSampY) * tCalp % iMeshFactor + 1
		allocate(rvTimeStamp(iNumRecords))
		
		! Extract a random sample of indices
		allocate(ivTotalIndex(iNx*iNy), ivPartialIndexX(iNx*iNy), ivPartialIndexY(iNx*iNy))
		allocate(ivSampleIndex(tOpts % iSampleSize), ivSampleX(tOpts % iSampleSize), ivSampleY(tOpts % iSampleSize))
		k = 0
		do i = 1, iNx
			do j = 1, iNy
				k = k + 1
				ivTotalIndex(k)    = k
				ivPartialIndexX(k) = i
				ivPartialIndexY(k) = j
			end do
		end do
		call ransam(ivTotalIndex, ivSampleIndex)
		ivSampleX = ivPartialIndexX(ivSampleIndex)
		ivSampleY = ivPartialIndexY(ivSampleIndex)

		! Iterate over all file contents, and get comparison data
		allocate(rmSampleConcA(iNumRecords, tOpts % iSampleSize), rmSampleConcB(iNumRecords, tOpts % iSampleSize))
		do i = 1, iNumRecords
		
			! Gather concntration fields
			iRetCode = tCalp % getRecord(10, tOpts, .true.)
			if(iRetCode /= 0) exit
			iRetCode = tCalp2 % getRecord(11, tOpts, .true.)
			if(iRetCode /= 0) exit
			iYear   = tCalp % iYear
			iJulDay = tCalp % iJulDay
			iHour   = tCalp % iHour
			tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
			rvTimeStamp(i) = tDateTime % toEpoch() + (iJulDay - 1) * 86400.d0 + iHour * 3600.d0
			
			! Store results
			do j = 1, tOpts % iSampleSize
				rmSampleConcA(i, j) = tCalp % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
				rmSampleConcB(i, j) = tCalp2 % rmConc(ivSampleX(j), ivSampleY(j), tOpts % iSpeciesIdx)
			end do
			
		end do
		
		! Print results
		do j = 1, tOpts % iSampleSize

			! Generate actual file name based on prefix and positional indices,
			! then write data to it
			write(sCompFileName, "(a, 2('_', i4.4), '.csv')") trim(tOpts % sOutputFile), ivSampleX(j), ivSampleY(j)
			open(13, file=sCompFileName, status='unknown', action='write')
			write(13, "('Time.Stamp, First.File.Value, Second.File.Value')")
			do i = 1, iNumRecords
				write(13, "(i6, 2(',', e15.7))") i, rmSampleConcA(i, j), rmSampleConcB(i, j)
			end do
			close(13)
			
		end do
		close(12)

	case default

		! Inform user
		print *, 'Unknown option specified: ', tOpts % iProcessingType

    end select


contains

	  function co_m8(ivDay, ivHour, raValues, raMaxMvAvg8, lDebug) result(iRetCode)

	    ! Routine arguments
	    integer, dimension(:), intent(in)                 :: ivDay
	    integer, dimension(:), intent(in)                 :: ivHour
	    real, dimension(:,:,:), intent(in)                :: raValues
	    real, dimension(:,:,:), allocatable, intent(out)  :: raMaxMvAvg8
	    logical, intent(in), optional                     :: lDebug
	    integer                                           :: iRetCode

	    ! Locals
	    integer :: iMinDay
	    integer :: iMaxDay
	    integer :: iNumDays
	    integer :: iDayIdx
	    integer :: i
	    integer :: iNx
	    integer :: iNy
	    integer :: iX
	    integer :: iY
	    integer :: n
	    logical :: lPrintReport
	    integer, dimension(size(ivDay))                                           :: ivBegin
	    integer, dimension(size(ivDay))                                           :: ivEnd
	    real, dimension(size(ivDay),size(raValues, dim=2),size(raValues, dim=3))  :: raMeans

	    ! Assume success (will falsify on failure)
	    iRetCode = 0

	    ! Check parameters
	    iNx = size(raValues, dim=2)
	    iNy = size(raValues, dim=3)

	    ! Compute day extrema and use this information to reserve workspace
	    iMinDay = minval(ivDay)
	    iMaxDay = maxval(ivDay)
	    iNumDays = iMaxDay - iMinDay + 1
	    if(allocated(raMaxMvAvg8)) deallocate(raMaxMvAvg8)
	    allocate(raMaxMvAvg8(iNumDays, iNx, iNy))

	    ! Decide whether to debug-print or not
	    lPrintReport = .false.
	    if(present(lDebug)) then
	      lPrintReport = lDebug
	    end if

	    ! Compute the moving averages
	    n = size(ivDay)
	    do i = 1, n
	      ivBegin(i) = max(i-7, 1)
	      ivEnd(i)   = min(i, n)
	      do iX = 1, iNx
	        do iY = 1, iNy
	          raMeans(i, iX, iY) = sum(raValues(ivBegin(i):ivEnd(i), iX, iY)) / (ivEnd(i) - ivBegin(i) + 1)
	        end do
	      end do
	      if(lPrintReport) then
	        print "('AVG> ', i5, ':', i2.2, 2(1x,e15.7))", ivDay(i), ivHour(i), minval(raMeans(i, :, :)), maxval(raMeans(i, :, :))
	      end if
	    end do

	    ! Compute the daily maxima
	    raMaxMvAvg8 = 0.0
	    do i = 1, n
	      iDayIdx = ivDay(i) - iMinDay + 1
	      do iX = 1, iNx
	        do iY = 1, iNy
	          raMaxMvAvg8(iDayIdx, iX, iY) = max(raMaxMvAvg8(iDayIdx, iX, iY), raMeans(i, iX, iY))
	        end do
	      end do
	      if(lPrintReport) then
	        print "('MAX> ', i5, ':', i2.2, 2(1x,e15.7))", ivDay(i), ivHour(i), minval(raMeans(i, :, :)), maxval(raMeans(i, :, :))
	      end if
	    end do

	  end function co_m8

    subroutine ErrorExit(sMessage, sErrMsg, iRetCode)

        ! Routine arguments
        character(len=*), intent(in)    		:: sMessage
        character(len=*), intent(in), optional	:: sErrMsg
        integer, intent(in), optional			:: iRetCode

        ! Locals
        ! -none-

        ! print error message and exit
        if(present(iRetCode) .and. .not.present(sErrMsg)) then
			print *,'ConcDecode: error: ', TRIM(sMessage), ' - Return code: ',iRetCode
        elseif(.not.present(iRetCode) .and. present(sErrMsg)) then
			print *,'ConcDecode: error: ', TRIM(sMessage), trim(sErrMsg)
        elseif(present(iRetCode) .and. present(sErrMsg)) then
			print *,'ConcDecode: error: ', TRIM(sMessage), ' - ',iRetCode, trim(sErrMsg)
        else
			print *,'ConcDecode: error: ', TRIM(sMessage)
		end if
        stop

    end subroutine ErrorExit
    
    
    ! Refactoring and translation of the program in
    !
    !	B. F. Green, "FORTRAN subroutines for random sampling
    ! 	without replacement", Behavior Research Methods &
    !	Instrumentation, 9(6), p.559, 1977
    !
    ! itself a direct coding of the random sampling algorithm in
    ! D.E. Knuth, The Art of Computer Programming, Volume 2,
    ! Seminumerical Algorithms, Addison-Wesley
    !
    subroutine ransam(x,a)
    
		! Routine arguments
		integer, dimension(:), intent(in)	:: x	! Vector to sample
		integer, dimension(:), intent(out)	:: a	! Output sample
		
		! Locals
		integer		:: j
		integer		:: k
		integer		:: l
		integer		:: m
		integer		:: n
		real		:: random_value
		
		! Get array sizes
		n = size(x)
		k = size(a)
		
		! Perform actual sample selection
		m = 0
		do j = 1, n
			call random_number(random_value)
			l = int((float(n-j+1))*random_value) + 1
			if(l <= k-m) then
				m = m + 1
				a(m) = x(j)
				if(m >= k) return
			end if
		end do
    
    end subroutine ransam

end program ConcDecode
