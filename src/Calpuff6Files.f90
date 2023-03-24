! CalpuffFiles - Module supporting Calpuff 6+ file formats
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
module Calpuff6Files

	use calendar
	use options

	implicit none
	
	private
	
	! Public interface
	public	:: CalmetType
	public	:: CalpuffType
	public	:: WriteDataToCalpuffCtdm
	public	:: WriteDataToCaline
	public	:: WriteDataToStandardProfile
	
	! CALMET data, as from official documents
	type CalmetType
		! Header
		character(16)								:: sDataSet
		character(16)								:: sDataVer
		character(64)								:: sDataMod
		character(132), dimension(:), allocatable	:: svComment
		integer										:: iIbyr
		integer										:: iIbmo
		integer										:: iIbdy
		integer										:: iIbhr
		integer										:: iJbtz
		integer										:: iIbsec
		integer										:: iIeyr
		integer										:: iIemo
		integer										:: iIedy
		integer										:: iIehr
		integer										:: iIesec
		character(8)								:: sAxtz
		integer										:: iIrlg
		integer										:: iIrtype
		integer										:: iNx
		integer										:: iNy
		integer										:: iNz
		real										:: rDgrid
		integer										:: iIwfcod
		real										:: rXorigr
		real										:: rYorigr
		integer										:: iNssta
		integer										:: iNusta
		integer										:: iNpsta
		integer										:: iNowsta
		integer										:: iNlu
		integer										:: iIwat1
		integer										:: iIwat2
		logical										:: lLcalgrd
		character(8)								:: sPmap
		character(8)								:: sDatum
		character(12)								:: sDaten
		real										:: rFeast
		real										:: rFnorth
		character(4)								:: sUtmhem
		integer										:: iUtmzn
		real										:: rRnlat0
		real										:: rRelon0
		real										:: rXlat1
		real										:: rXlat2
		character(8)								:: sClab1
		real, dimension(:), allocatable				:: rvZfacem
		character(8)								:: sClab2
		real, dimension(:), allocatable				:: rvXssta
		character(8)								:: sClab3
		real, dimension(:), allocatable				:: rvYssta
		character(8)								:: sClab4
		real, dimension(:), allocatable				:: rvXusta
		character(8)								:: sClab5
		real, dimension(:), allocatable				:: rvYusta
		character(8)								:: sClab6
		real, dimension(:), allocatable				:: rvXpsta
		character(8)								:: sClab7
		real, dimension(:), allocatable				:: rvYpsta
		character(8)								:: sClab8
		real, dimension(:,:), allocatable			:: rmZ0
		character(8)								:: sClab9
		integer, dimension(:,:), allocatable		:: imIlandu
		character(8)								:: sClab10
		real, dimension(:,:), allocatable			:: rmElev
		character(8)								:: sClab11
		real, dimension(:,:), allocatable			:: rmXlai
		character(8)								:: sClab12
		integer, dimension(:,:), allocatable		:: imNears
		! Deduced information
		integer										:: iDeltaTime
		! Record
		logical										:: lHasBeenRead
		character(8)								:: sClabu
		integer										:: iNdathrb
		integer										:: iNdathre
		real, dimension(:,:,:), allocatable			:: raU
		character(8)								:: sClabv
		real, dimension(:,:,:), allocatable			:: raV
		character(8)								:: sClabw
		real, dimension(:,:,:), allocatable			:: raW
		character(8)								:: sClabt
		real, dimension(:,:,:), allocatable			:: raZtemp
		character(8)								:: sClabsc
		integer, dimension(:,:), allocatable		:: imIpgt
		character(8)								:: sClabus
		real, dimension(:,:), allocatable			:: rmUstar
		character(8)								:: sClabzi
		real, dimension(:,:), allocatable			:: rmZi
		character(8)								:: sClabl
		real, dimension(:,:), allocatable			:: rmEl
		character(8)								:: sClabws
		real, dimension(:,:), allocatable			:: rmWstar
		character(8)								:: sClabrmm
		real, dimension(:,:), allocatable			:: rmRmm
		character(8)								:: sClabtk
		real, dimension(:,:), allocatable			:: rmTempk
		character(8)								:: sClabd
		real, dimension(:,:), allocatable			:: rmRho
		character(8)								:: sClabq
		real, dimension(:,:), allocatable			:: rmQsw
		character(8)								:: sClabrh
		integer, dimension(:,:), allocatable		:: imIrh
		character(8)								:: sClabpc
		integer, dimension(:,:), allocatable		:: imIpcode
	contains
		procedure	:: cleanHeader => cleanHeader
		procedure	:: cleanRecord => cleanRecord
		procedure	:: getHeader   => getHeader
		procedure	:: getRecord   => getRecord
		procedure	:: putHeader   => putHeader
		procedure	:: putRecord   => putRecord
		procedure	:: zeroW       => zeroW
		procedure	:: openGrADS   => OpenGrADS
		procedure	:: closeGrADS  => CloseGrADS
		procedure	:: writeGrADS  => WriteGrADS
	end type CalmetType
	
	type CalpuffType
		! Meta-data
		integer											:: iCpuffMode	! 6 or 7, as of Calpuff version used to produce data (which users should know in advance)
		! Header
		character(len=16)                               :: sDataSet
		character(len=16)                               :: sDataVersion
		character(len=64)                               :: sDataModel
		character(len=132), dimension(:), allocatable	:: svCommentLine
		character(len=12)                               :: sModel
		character(len=12)                               :: sVersion
		character(len=12)                               :: sLevel
		integer                                         :: iBeginYear
		integer                                         :: iBeginJulday
		integer                                         :: iBeginHour
		integer                                         :: iBeginSecond
		character(len=8)                                :: sTimeZone
		integer                                         :: iLoggingTime
		integer                                         :: iModelSteps
		integer                                         :: iAveragingTime
		integer                                         :: iNx
		integer                                         :: iNy
		real                                            :: rDx
		real                                            :: rDy
		integer                                         :: iNumLevels
		real                                            :: rX0
		real                                            :: rY0
		integer                                         :: iNumSurfaceStations
		integer                                         :: iMinCompX
		integer                                         :: iMaxCompX
		integer                                         :: iMinCompY
		integer                                         :: iMaxCompY
		integer                                         :: iMinSampX
		integer                                         :: iMaxSampX
		integer                                         :: iMinSampY
		integer                                         :: iMaxSampY
		integer                                         :: iMeshFactor
		integer											:: iNumSourceTypes
		integer                                         :: iNumPointSources1
		integer                                         :: iNumAreaSources1
		integer                                         :: iNumLineSources1
		integer                                         :: iNumVolumeSources1
		integer                                         :: iNumPointSources2
		integer                                         :: iNumAreaSources2
		integer                                         :: iNumLineSources2
		integer                                         :: iNumVolumeSources2
		integer                                         :: iMsource
		integer                                         :: iNumFreeReceptors
		integer                                         :: iNumReceptorGroups
		integer                                         :: iNumCTSGReceptors
		logical                                         :: lSamplingGrid
		integer                                         :: iNumChemicalSpecies
		logical                                         :: lCompressed
		integer                                         :: i2Dmet
		integer                                         :: iUTMZone
		real                                            :: rFalseEasting
		real                                            :: rFalseNorthing
		real                                            :: rNlat0
		real                                            :: rElon0
		real                                            :: rXlat1
		real                                            :: rXlat2
		character(len=8)                                :: sPmap
		character(len=4)                                :: sUTMHem
		character(len=8)                                :: sDatum
		character(len=12)                               :: sDaten
		character(len=16)                               :: sClat0
		character(len=16)                               :: sClon0
		character(len=16)                               :: sClat1
		character(len=16)                               :: sClat2
		character(len=15), dimension(:), allocatable    :: svSpeciesName
		character(len=16), dimension(:), allocatable    :: svSpeciesUnit
		real, dimension(:), allocatable                 :: rvFreeX
		real, dimension(:), allocatable                 :: rvFreeY
		real, dimension(:), allocatable                 :: rvFreeZ
		real, dimension(:), allocatable                 :: rvFreeHeight
		integer, dimension(:), allocatable              :: ivFreeGroupIdx
		character(len=80), dimension(:), allocatable	:: svFreeGroup
		real, dimension(:), allocatable                 :: rvCTSGX
		real, dimension(:), allocatable                 :: rvCTSGY
		real, dimension(:), allocatable                 :: rvCTSGZ
		integer, dimension(:), allocatable              :: ivHill
		character(len=80), dimension(3)					:: svTitle
		character(len=256), dimension(:), allocatable   :: svSeriesFile
		character(len=16), dimension(:), allocatable    :: svPointSource1
		character(len=16), dimension(:), allocatable    :: svPointSource2
		character(len=16), dimension(:), allocatable    :: svAreaSource1
		character(len=16), dimension(:), allocatable    :: svAreaSource2
		character(len=16), dimension(:), allocatable    :: svLineSource1
		character(len=16), dimension(:), allocatable    :: svLineSource2
		character(len=16), dimension(:), allocatable    :: svVolumeSource1
		character(len=16), dimension(:), allocatable    :: svVolumeSource2
		character(len=16), dimension(:,:), allocatable	:: smSourceName
		logical											:: lHasBeenRead
		integer, dimension(:), allocatable				:: ivNumSourcesPerType
		! Deduced information
		integer                                         :: iNxSampling
		integer                                         :: iNySampling
		! Record
		integer                                         :: iYearDisplay
		integer                                         :: iMonthDisplay
		integer                                         :: iDayDisplay
		integer                                         :: iHourDisplay
		integer                                         :: iMinuteDisplay
		integer                                         :: iSecondDisplay
		integer                                         :: iYear
		integer                                         :: iJulday
		integer                                         :: iHour
		integer                                         :: iSecond
		integer                                         :: iYear2
		integer                                         :: iJulday2
		integer                                         :: iHour2
		integer                                         :: iSecond2
		integer                                         :: iSrcType
		integer                                         :: iSrcNum
		character(len=16)								:: sSrcName
		real											:: rSrcX, rSrcY
		real, dimension(:,:), allocatable               :: rvCTSGConc
		real, dimension(:), allocatable                 :: rvCTSGCompressedConc
		real, dimension(:,:), allocatable               :: rvFreeConc
		real, dimension(:), allocatable                 :: rvFreeCompressedConc
		real, dimension(:,:,:), allocatable             :: rmConc
		integer, dimension(:,:,:), allocatable          :: imExceedances
		! Gridded accumulators
		integer											:: iNumConc
		real, dimension(:,:,:), allocatable             :: rmSumConc
		real, dimension(:,:,:), allocatable             :: rmSum2Conc
		real, dimension(:,:,:), allocatable             :: rmAvgConc
		real, dimension(:,:,:), allocatable             :: rmStdConc
		real, dimension(:,:,:), allocatable             :: rmMaxConc
		integer, dimension(:,:,:), allocatable          :: imMaxPos
		real, dimension(:), allocatable                 :: rvConc
		real, dimension(:), allocatable                 :: rvCompressedData
		! Gridded accumulators, temporary
		real, dimension(:,:,:), allocatable				:: rtMaxConc
		real, dimension(:,:), allocatable				:: rmMinConc
		integer, dimension(:,:,:), allocatable			:: imMinConcIdx
		! Synthesis
		integer                                         :: iNumNonzeroConc
		real											:: rConcRatio
		real											:: rMeanNonzeroConc
	contains
		procedure	:: cleanHeader      => cleanCalpuffHeader
		procedure	:: cleanRecord      => cleanCalpuffRecord
		procedure	:: getHeader        => getCalpuffHeader
		procedure	:: getRecord        => getCalpuffRecord
		procedure	:: writeGridSpec    => writeCalpuffGridSpec
		procedure	:: writeGrid        => writeCalpuffGrid
		procedure	:: open01           => openCalpuff01
		procedure	:: write01          => writeCalpuff01
		procedure	:: writeComment     => writeCalpuffComment
		procedure	:: writeHeader      => writeCalpuffHeader
		procedure	:: openSeries       => openCalpuffSeries
		procedure	:: writeSeries      => writeCalpuffSeries
		procedure	:: openMultiSeries  => openCalpuffMultiSeries
		procedure	:: writeMultiSeries => writeCalpuffMultiSeries
		procedure	:: openMultiSeriesClosest  => openCalpuffMultiSeriesClosest
		procedure	:: writeMultiSeriesClosest => writeCalpuffMultiSeriesClosest
		procedure	:: openSummary      => openCalpuffSummary
		procedure	:: writeSummary     => writeCalpuffSummary
		procedure	:: closeSummary     => closeCalpuffSummary
		procedure	:: openMaxima       => openCalpuffMaxima
		procedure	:: writeMaxima      => writeCalpuffMaxima
		procedure	:: updateExceeds    => updateCalpuffExceedances
		procedure	:: resetExceeds     => resetCalpuffExceedances
		procedure	:: writeExceeds     => writeCalpuffExceedances
		procedure	:: startQuantile    => startCalpuffQuantile
		procedure	:: updateQuantile   => updateCalpuffQuantile
		procedure	:: writeQuantile    => writeCalpuffQuantile
		procedure	:: writeMean        => writeCalpuffMean
		procedure	:: writeMax         => writeCalpuffMax
		procedure	:: writeConc        => writeCalpuffConc
	end type CalpuffType
	
contains

	subroutine cleanHeader(this)
	
		! Routine arguments
		class(CalmetType), intent(inout)	:: this
		
		! Locals
		! -none-
		
		! Reclaim allocated space, if any
		if(allocated(this % svComment)) deallocate(this % svComment)
		if(allocated(this % rvZfacem)) deallocate(this % rvZfacem)
		if(allocated(this % rvXssta)) deallocate(this % rvXssta)
		if(allocated(this % rvYssta)) deallocate(this % rvYssta)
		if(allocated(this % rvXusta)) deallocate(this % rvXusta)
		if(allocated(this % rvYusta)) deallocate(this % rvYusta)
		if(allocated(this % rvXpsta)) deallocate(this % rvXpsta)
		if(allocated(this % rvYpsta)) deallocate(this % rvYpsta)
		if(allocated(this % rmZ0)) deallocate(this % rmZ0)
		if(allocated(this % imIlandu)) deallocate(this % imIlandu)
		if(allocated(this % rmElev)) deallocate(this % rmElev)
		if(allocated(this % rmXlai)) deallocate(this % rmXlai)
		if(allocated(this % imNears)) deallocate(this % imNears)
		
	end subroutine cleanHeader
	

	subroutine cleanRecord(this)
	
		! Routine arguments
		class(CalmetType), intent(inout)	:: this
		
		! Locals
		! -none-
		
		! Reclaim allocated space, if any
		if(allocated(this % raU)) deallocate(this % raU)
		if(allocated(this % raV)) deallocate(this % raV)
		if(allocated(this % raW)) deallocate(this % raW)
		if(allocated(this % raZtemp)) deallocate(this % raZtemp)
		if(allocated(this % imIpgt)) deallocate(this % imIpgt)
		if(allocated(this % rmUstar)) deallocate(this % rmUstar)
		if(allocated(this % rmZi)) deallocate(this % rmZi)
		if(allocated(this % rmEl)) deallocate(this % rmEl)
		if(allocated(this % rmWstar)) deallocate(this % rmWstar)
		if(allocated(this % rmRmm)) deallocate(this % rmRmm)
		if(allocated(this % rmTempk)) deallocate(this % rmTempk)
		if(allocated(this % rmRho)) deallocate(this % rmRho)
		if(allocated(this % rmQsw)) deallocate(this % rmQsw)
		if(allocated(this % imIrh)) deallocate(this % imIrh)
		if(allocated(this % imIpcode)) deallocate(this % imIpcode)
		
	end subroutine cleanRecord
	

	function getHeader(this, iLUN, sFileName) result(iRetCode)
		
		! Routine arguments
		class(CalmetType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sFileName
		integer								:: iRetCode
		
		! Local variables
		integer		:: iErrCode
		integer		:: iNcom
		integer		:: i
		integer		:: iDum
		integer		:: iTimeFrom
		integer		:: iTimeTo
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Try accessing file
		open(iLUN, file=sFileName, action='read', status='old', form='unformatted', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Clean current data set
		call this % cleanHeader()
		call this % cleanRecord()
		
		! File declaration
		read(iLUN, iostat=iErrCode) &
			this % sDataSet, this % sDataVer, this % sDataMod
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 2
			return
		end if
		
		! Comments
		read(iLUN, iostat=iErrCode) &
			iNcom
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 3
			return
		end if
		allocate(this % svComment(iNcom))
		do i = 1, iNcom
			read(iLUN, iostat=iErrCode) &
				this % svComment(i)
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 4
				return
			end if
		end do
		
		! Run control
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % iIbyr, this % iIbmo, this % iIbdy, this % iIbhr, this % iIbsec, &
				this % iIeyr, this % iIemo, this % iIedy, this % iIehr, this % iIesec, &
				this % sAxtz, this % iIrlg, this % iIrtype, &
				this % iNx, this % iNy, this % iNz, this % rDgrid, this % rXorigr, this % rYorigr, &
				this % iIwfcod, &
				this % iNssta, this % iNusta, this % iNpsta, this % iNowsta, &
				this % iNlu, this % iIwat1, this % iIwat2, &
				this % lLcalgrd, &
				this % sPmap, this % sDatum, this % sDaten, this % rFeast, this % rFnorth, &
				this % sUtmhem, this % iUtmzn, &
				this % rRnlat0, this % rRelon0, this % rXlat1, this % rXlat2
			call PackTime(iTimeFrom, this % iIbyr, this % iIbmo, this % iIbdy, this % iIbhr, 0, 0)
			iTimeFrom = iTimeFrom + this % iIbsec
			call PackTime(iTimeTo,   this % iIeyr, this % iIemo, this % iIedy, this % iIehr, 0, 0)
			iTimeTo = iTimeTo + this % iIesec
			this % iDeltaTime = (iTimeTo - iTimeFrom) / this % iIrlg
		else
			read(iLUN, iostat=iErrCode) &
				this % iIbyr, this % iIbmo, this % iIbdy, this % iIbhr, &
				this % iJbtz, this % iIrlg, this % iIrtype, &
				this % iNx, this % iNy, this % iNz, this % rDgrid, this % rXorigr, this % rYorigr, &
				this % iIwfcod, &
				this % iNssta, this % iNusta, this % iNpsta, this % iNowsta, &
				this % iNlu, this % iIwat1, this % iIwat2, &
				this % lLcalgrd, &
				this % sPmap, this % sDatum, this % sDaten, this % rFeast, this % rFnorth, &
				this % sUtmhem, this % iUtmzn, &
				this % rRnlat0, this % rRelon0, this % rXlat1, this % rXlat2
			call PackTime(iTimeFrom, this % iIbyr, this % iIbmo, this % iIbdy, this % iIbhr, 0, 0)
			call PackTime(iTimeTo,   this % iIeyr, this % iIemo, this % iIedy, this % iIehr, 0, 0)
			this % iDeltaTime = (iTimeTo - iTimeFrom) / this % iIrlg
		end if
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 5
			return
		end if
		
		! Cell face heights
		allocate(this % rvZfacem(this % iNz + 1))
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClab1, iDum, iDum, iDum, iDum, this % rvZfacem
		else
			read(iLUN, iostat=iErrCode) &
				this % sClab1, iDum, this % rvZfacem
		end if
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 6
			return
		end if
		
		! Surface stations
		if(this % iNssta >= 1) then
			allocate(this % rvXssta(this % iNssta))
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClab2, iDum, iDum, iDum, iDum, this % rvXssta
			else
				read(iLUN, iostat=iErrCode) &
					this % sClab2, iDum, this % rvXssta
			end if
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 7
				return
			end if
			allocate(this % rvYssta(this % iNssta))
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClab3, iDum, iDum, iDum, iDum, this % rvYssta
			else
				read(iLUN, iostat=iErrCode) &
					this % sClab3, iDum, this % rvYssta
			end if
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 8
				return
			end if
		end if
		
		! Upper air stations
		if(this % iNusta >= 1) then
			allocate(this % rvXusta(this % iNusta))
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClab4, iDum, iDum, iDum, iDum, this % rvXusta
			else
				read(iLUN, iostat=iErrCode) &
					this % sClab4, iDum, this % rvXusta
			end if
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 9
				return
			end if
			allocate(this % rvYusta(this % iNusta))
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClab5, iDum, iDum, iDum, iDum, this % rvYusta
			else
				read(iLUN, iostat=iErrCode) &
					this % sClab5, iDum, this % rvYusta
			end if
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 10
				return
			end if
		end if
		
		! Precipitation stations
		if(this % iNpsta >= 1) then
			allocate(this % rvXpsta(this % iNpsta))
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClab6, iDum, iDum, iDum, iDum, this % rvXpsta
			else
				read(iLUN, iostat=iErrCode) &
					this % sClab6, iDum, this % rvXpsta
			end if
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 11
				return
			end if
			allocate(this % rvYpsta(this % iNpsta))
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClab7, iDum, iDum, iDum, iDum, this % rvYpsta
			else
				read(iLUN, iostat=iErrCode) &
					this % sClab7, iDum, this % rvYpsta
			end if
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 12
				return
			end if
		end if
		
		! Surface roughness
		allocate(this % rmZ0(this % iNx, this % iNy))
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClab8, iDum, iDum, iDum, iDum, this % rmZ0
		else
			read(iLUN, iostat=iErrCode) &
				this % sClab8, iDum, this % rmZ0
		end if
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 13
			return
		end if
		
		! Land use
		allocate(this % imIlandu(this % iNx, this % iNy))
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClab9, iDum, iDum, iDum, iDum, this % imIlandu
		else
			read(iLUN, iostat=iErrCode) &
				this % sClab9, iDum, this % imIlandu
		end if
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 14
			return
		end if
		
		! Elevation
		allocate(this % rmElev(this % iNx, this % iNy))
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClab10, iDum, iDum, iDum, iDum, this % rmElev
		else
			read(iLUN, iostat=iErrCode) &
				this % sClab10, iDum, this % rmElev
		end if
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 15
			return
		end if
		
		! Leaf area index
		allocate(this % rmXlai(this % iNx, this % iNy))
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClab11, iDum, iDum, iDum, iDum, this % rmXlai
		else
			read(iLUN, iostat=iErrCode) &
				this % sClab11, iDum, this % rmXlai
		end if
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 16
			return
		end if
		
		! Closest station
		if(this % iNssta >= 1) then
			allocate(this % imNears(this % iNx, this % iNy))
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClab12, iDum, iDum, iDum, iDum, this % imNears
			else
				read(iLUN, iostat=iErrCode) &
					this % sClab12, iDum, this % imNears
			end if
			if(iErrCode /= 0) then
				close(iLUN)
				iRetCode = 17
				return
			end if
		end if
		
		! Allocate all "Record" fields, and clean preventively its contents
		allocate(this % raU(this % iNx, this % iNy, this % iNz))
		allocate(this % raV(this % iNx, this % iNy, this % iNz))
		allocate(this % raW(this % iNx, this % iNy, this % iNz))
		allocate(this % raZtemp(this % iNx, this % iNy, this % iNz))
		allocate(this % imIpgt(this % iNx, this % iNy))
		allocate(this % rmUstar(this % iNx, this % iNy))
		allocate(this % rmZi(this % iNx, this % iNy))
		allocate(this % rmEl(this % iNx, this % iNy))
		allocate(this % rmWstar(this % iNx, this % iNy))
		allocate(this % rmRmm(this % iNx, this % iNy))
		allocate(this % rmTempk(this % iNx, this % iNy))
		allocate(this % rmRho(this % iNx, this % iNy))
		allocate(this % rmQsw(this % iNx, this % iNy))
		allocate(this % imIrh(this % iNx, this % iNy))
		allocate(this % imIpcode(this % iNx, this % iNy))
		this % raU      = 0.
		this % raV      = 0.
		this % raW      = 0.
		this % raZtemp  = 0.
		this % imIpgt   = 0
		this % rmUstar  = 0.
		this % rmZi     = 0.
		this % rmEl     = 0.
		this % rmWstar  = 0.
		this % rmRmm    = 0.
		this % rmTempk  = 0.
		this % rmRho    = 0.
		this % rmQsw    = 0.
		this % imIrh    = 0
		this % imIpcode = 0
		this % lHasBeenRead = .false.
		
	end function getHeader


	function getRecord(this, iLUN) result(iRetCode)
		
		! Routine arguments
		class(CalmetType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		integer								:: iRetCode
		
		! Local variables
		integer		:: iErrCode
		integer		:: k
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Wind field
		do k = 1, this % iNz
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % raU(:,:,k)
			else
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, &
					this % raU(:,:,k)
			end if
			if(iErrCode /= 0) then
				iRetCode = 1
				close(iLUN)
				return
			end if
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % raV(:,:,k)
			else
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, &
					this % raV(:,:,k)
			end if
			if(iErrCode /= 0) then
				iRetCode = 2
				close(iLUN)
				return
			end if
			if(this % lLcalgrd) then
				if(this % sDataVer == "2.1") then
					read(iLUN, iostat=iErrCode) &
						this % sClabu, &
						this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
						this % raW(:,:,k)
				else
					read(iLUN, iostat=iErrCode) &
						this % sClabu, &
						this % iNdathrb, &
						this % raW(:,:,k)
				end if
				if(iErrCode /= 0) then
					iRetCode = 3
					close(iLUN)
					return
				end if
			end if
		end do
		
		! Temperature
		if(this % lLcalgrd) then
			do k = 1, this % iNz
				if(this % sDataVer == "2.1") then
					read(iLUN, iostat=iErrCode) &
						this % sClabu, &
						this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
						this % raZtemp(:,:,k)
				else
					read(iLUN, iostat=iErrCode) &
						this % sClabu, &
						this % iNdathrb, &
						this % raZtemp(:,:,k)
				end if
				if(iErrCode /= 0) then
					iRetCode = 4
					close(iLUN)
					return
				end if
			end do
		end if
		
		! Stability categories
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % imIpgt
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % imIpgt
		end if
		if(iErrCode /= 0) then
			iRetCode = 5
			close(iLUN)
			return
		end if
		
		! Friction velocity
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmUstar
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmUstar
		end if
		if(iErrCode /= 0) then
			iRetCode = 6
			close(iLUN)
			return
		end if
		
		! Mixing height
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmZi
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmZi
		end if
		if(iErrCode /= 0) then
			iRetCode = 7
			close(iLUN)
			return
		end if
		
		! Obukhov length
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmEl
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmEl
		end if
		if(iErrCode /= 0) then
			iRetCode = 8
			close(iLUN)
			return
		end if
		
		! Deardoff velocity
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmWstar
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmWstar
		end if
		if(iErrCode /= 0) then
			iRetCode = 9
			close(iLUN)
			return
		end if
		
		! Precipitation rate
		if(this % iNpsta > 0) then
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % rmRmm
			else
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, &
					this % rmRmm
			end if
			if(iErrCode /= 0) then
				iRetCode = 10
				close(iLUN)
				return
			end if
		end if
		
		! Near-surface temperature
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmTempk
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmTempk
		end if
		if(iErrCode /= 0) then
			iRetCode = 11
			close(iLUN)
			return
		end if
		
		! Near-surface air density
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmRho
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmRho
		end if
		if(iErrCode /= 0) then
			iRetCode = 12
			close(iLUN)
			return
		end if
		
		! Global solar radiation
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmQsw
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmQsw
		end if
		if(iErrCode /= 0) then
			iRetCode = 13
			close(iLUN)
			return
		end if
		
		! Near-surface relative humidity
		if(this % sDataVer == "2.1") then
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % imIrh
		else
			read(iLUN, iostat=iErrCode) &
				this % sClabu, &
				this % iNdathrb, &
				this % imIrh
		end if
		if(iErrCode /= 0) then
			iRetCode = 14
			close(iLUN)
			return
		end if
		
		! Precipitation code
		if(this % iNpsta > 0) then
			if(this % sDataVer == "2.1") then
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % imIpcode
			else
				read(iLUN, iostat=iErrCode) &
					this % sClabu, &
					this % iNdathrb, &
					this % imIpcode
			end if
			if(iErrCode /= 0) then
				iRetCode = 15
				close(iLUN)
				return
			end if
		end if
		
	end function getRecord
		

	function putHeader(this, iLUN, sFileName) result(iRetCode)
		
		! Routine arguments
		class(CalmetType), intent(in)	:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sFileName
		integer							:: iRetCode
		
		! Local variables
		integer		:: iNcom
		integer		:: i
		integer		:: iDum
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Try accessing file
		open(iLUN, file=sFileName, action='write', status='unknown', form='unformatted')
		
		! File declaration
		write(iLUN) this % sDataSet, this % sDataVer, this % sDataMod
		
		! Comments
		write(iLUN) size(this % svComment)
		do i = 1, size(this % svComment)
			write(iLUN) this % svComment(i)
		end do
		
		! Run control
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % iIbyr, this % iIbmo, this % iIbdy, this % iIbhr, this % iIbsec, &
				this % iIeyr, this % iIemo, this % iIedy, this % iIehr, this % iIesec, &
				this % sAxtz, this % iIrlg, this % iIrtype, &
				this % iNx, this % iNy, this % iNz, this % rDgrid, this % rXorigr, this % rYorigr, &
				this % iIwfcod, &
				this % iNssta, this % iNusta, this % iNpsta, this % iNowsta, &
				this % iNlu, this % iIwat1, this % iIwat2, &
				this % lLcalgrd, &
				this % sPmap, this % sDatum, this % sDaten, this % rFeast, this % rFnorth, &
				this % sUtmhem, this % iUtmzn, &
				this % rRnlat0, this % rRelon0, this % rXlat1, this % rXlat2
		else
			write(iLUN) &
				this % iIbyr, this % iIbmo, this % iIbdy, this % iIbhr, &
				this % iJbtz, this % iIrlg, this % iIrtype, &
				this % iNx, this % iNy, this % iNz, this % rDgrid, this % rXorigr, this % rYorigr, &
				this % iIwfcod, &
				this % iNssta, this % iNusta, this % iNpsta, this % iNowsta, &
				this % iNlu, this % iIwat1, this % iIwat2, &
				this % lLcalgrd, &
				this % sPmap, this % sDatum, this % sDaten, this % rFeast, this % rFnorth, &
				this % sUtmhem, this % iUtmzn, &
				this % rRnlat0, this % rRelon0, this % rXlat1, this % rXlat2
		end if
		
		! Cell face heights
		if(this % sDataVer == "2.1") then
			write(iLUN) this % sClab1, this % rvZfacem
		else
			write(iLUN) this % sClab1, iDum, this % rvZfacem
		end if
		
		! Surface stations
		iDum = 0
		if(this % sDataVer == "2.1") then
			if(this % iNssta >= 1) then
				write(iLUN) this % sClab2, iDum, iDum, iDum, iDum, this % rvXssta
				write(iLUN) this % sClab3, iDum, iDum, iDum, iDum, this % rvYssta
			end if
		else
			if(this % iNssta >= 1) then
				write(iLUN) this % sClab2, iDum, this % rvXssta
				write(iLUN) this % sClab3, iDum, this % rvYssta
			end if
		end if
		
		! Upper air stations
		if(this % sDataVer == "2.1") then
			if(this % iNusta >= 1) then
				write(iLUN) this % sClab4, iDum, iDum, iDum, iDum, this % rvXusta
				write(iLUN) this % sClab5, iDum, iDum, iDum, iDum, this % rvYusta
			end if
		else
			if(this % iNusta >= 1) then
				write(iLUN) this % sClab4, iDum, this % rvXusta
				write(iLUN) this % sClab5, iDum, this % rvYusta
			end if
		end if
		
		! Precipitation stations
		if(this % sDataVer == "2.1") then
			if(this % iNpsta >= 1) then
				write(iLUN) this % sClab6, iDum, iDum, iDum, iDum, this % rvXpsta
				write(iLUN) this % sClab7, iDum, iDum, iDum, iDum, this % rvYpsta
			end if
		else
			if(this % iNpsta >= 1) then
				write(iLUN) this % sClab6, iDum, this % rvXpsta
				write(iLUN) this % sClab7, iDum, this % rvYpsta
			end if
		end if
		
		! Surface roughness
		if(this % sDataVer == "2.1") then
			write(iLUN) this % sClab8, iDum, iDum, iDum, iDum, this % rmZ0
		else
			write(iLUN) this % sClab8, iDum, this % rmZ0
		end if
		
		! Land use
		if(this % sDataVer == "2.1") then
			write(iLUN) this % sClab9, iDum, iDum, iDum, iDum, this % imIlandu
		else
			write(iLUN) this % sClab9, iDum, this % imIlandu
		end if
		
		! Elevation
		if(this % sDataVer == "2.1") then
			write(iLUN) this % sClab10, iDum, iDum, iDum, iDum, this % rmElev
		else
			write(iLUN) this % sClab10, iDum, this % rmElev
		end if
		
		! Leaf area index
		if(this % sDataVer == "2.1") then
			write(iLUN) this % sClab11, iDum, iDum, iDum, iDum, this % rmXlai
		else
			write(iLUN) this % sClab11, iDum, this % rmXlai
		end if
		
		! Closest station
		if(this % iNssta >= 1) then
			if(this % sDataVer == "2.1") then
				write(iLUN) this % sClab12, iDum, iDum, iDum, iDum, this % imNears
			else
				write(iLUN) this % sClab12, iDum, this % imNears
			end if
		end if
		
	end function putHeader


	function putRecord(this, iLUN) result(iRetCode)
		
		! Routine arguments
		class(CalmetType), intent(in)	:: this
		integer, intent(in)				:: iLUN
		integer							:: iRetCode
		
		! Local variables
		integer		:: k
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Wind field
		if(this % sDataVer == "2.1") then
			do k = 1, this % iNz
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % raU(:,:,k)
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % raV(:,:,k)
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % raW(:,:,k)
			end do
		else
			do k = 1, this % iNz
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, &
					this % raU(:,:,k)
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, &
					this % raV(:,:,k)
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, &
					this % raW(:,:,k)
			end do
		end if
		
		! Temperature
		if(this % lLcalgrd) then
			if(this % sDataVer == "2.1") then
				do k = 1, this % iNz
					write(iLUN) &
						this % sClabu, &
						this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
						this % raZtemp(:,:,k)
				end do
			else
				do k = 1, this % iNz
					write(iLUN) &
						this % sClabu, &
						this % iNdathrb, &
						this % raZtemp(:,:,k)
				end do
			end if
		end if
		
		! Stability categories
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % imIpgt
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % imIpgt
		end if
		
		! Friction velocity
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmUstar
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmUstar
		end if
		
		! Mixing height
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmZi
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmZi
		end if
		
		! Obukhov length
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmEl
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmEl
		end if
		
		! Deardoff velocity
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmWstar
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmWstar
		end if
		
		! Precipitation rate
		if(this % iNpsta > 0) then
			if(this % sDataVer == "2.1") then
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % rmRmm
			else
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, &
					this % rmRmm
			end if
		end if
		
		! Near-surface temperature
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmTempk
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmTempk
		end if
		
		! Near-surface air density
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmRho
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmRho
		end if
		
		! Global solar radiation
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % rmQsw
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % rmQsw
		end if
		
		! Near-surface relative humidity
		if(this % sDataVer == "2.1") then
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
				this % imIrh
		else
			write(iLUN) &
				this % sClabu, &
				this % iNdathrb, &
				this % imIrh
		end if
		
		! Precipitation code
		if(this % iNpsta > 0) then
			if(this % sDataVer == "2.1") then
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, this % iIbsec, this % iNdathre, this % iIesec, &
					this % imIpcode
			else
				write(iLUN) &
					this % sClabu, &
					this % iNdathrb, &
					this % imIpcode
			end if
		end if
		
	end function putRecord
	
	
	subroutine zeroW(this)
	
		! Routine arguments
		class(CalmetType), intent(inout)	:: this
		
		! Locals
		! -none-
		
		! Set vertical wind speed to 0
		this % raW = 0.
		
	end subroutine zeroW
	
	
	function OpenGrADS(this, iLUN_C, iLUN_D, sGrADSFile, sGrADSCtl) result(iRetCode)
	
		! Routine arguments
		class(CalmetType), intent(inout)	:: this
		integer, intent(in)					:: iLUN_C, iLUN_D
		character(len=*), intent(in)		:: sGrADSFile
		character(len=*), intent(in)		:: sGrADSCtl
		integer								:: iRetCode
		
		! Locals
		integer							:: iErrCode
		real, dimension(:), allocatable	:: rvPresLevel
		character(len=16)				:: sTimeStamp
		integer							:: i
		
		! Constants
		character(len=3), dimension(12), parameter	:: svMonth = [ &
			'jan', 'feb', 'mar', 'apr', &
			'may', 'jun', 'jul', 'aug', &
			'sep', 'oct', 'nov', 'dec'  &
		]
		
		! Assume success (will falsify in case of failure)
		iRetCode = 0
		
		! Generate pressure levels corresponding to altitudes (will be written to CTL file, 'ZDEF' line)
		! (no special accuracy needed - levels are just for visual reference)
		allocate(rvPresLevel(this % iNz))
		do i = 1, this % iNz
			rvPresLevel(i) = 1013.0 * exp(-9.807 * 0.0289644 * this % rvZfacem(i) /(8.31432*293.15))
		end do
		
		! Generate time stamp (will be written to CTL file, 'TDEF' line)
		write(sTimeStamp, "(i2.2,'z',i2.2,a,i4.4)") &
			this % iIbhr, &
			this % iIbdy, &
			svMonth(this % iIbmo), &
			this % iIbyr
		
		! Create CTL from prefix information
		open(iLUN_C, file=sGrADSCtl, status='unknown', action='write')
		write(iLUN_C, "('DSET ^',a)") trim(sGrADSFile)
		write(iLUN_C, "('TITLE Data converted from CALMET file')")
		write(iLUN_C, "('OPTIONS sequential')")
		write(iLUN_C, "('UNDEF 0.10000E+16')")
		write(iLUN_C, "('XDEF ',i5,' linear ',f9.1,1x,f7.3)") this % iNx, this % rXorigr, this % rDgrid
		write(iLUN_C, "('YDEF ',i5,' linear ',f9.1,1x,f7.3)") this % iNy, this % rYorigr, this % rDgrid
		write(iLUN_C, "('ZDEF ',i5,' levels',50(1x,i4))") this % iNz, (int(rvPresLevel(i)), i=1, this % iNz)
		write(iLUN_C, "('TDEF ',i5,' linear ',a,' 1hr')") this % iIrlg, trim(sTimeStamp)
		write(iLUN_C, "('VARS 4')")
		write(iLUN_C, "('u ',i5,' 99 U component')") this % iNz
		write(iLUN_C, "('v ',i5,' 99 V component')") this % iNz
		write(iLUN_C, "('w ',i5,' 99 W component')") this % iNz
		write(iLUN_C, "('Temp ',i5,' 99 Temperature')") this % iNz
		write(iLUN_C, "('ENDVARS')")
		close(iLUN_C)
		deallocate(rvPresLevel)
		
		! Open data file
		open( &
			iLUN_D, &
			file=sGrADSFile, &
			status='unknown', &
			action='write', &
			form='unformatted' &
		)
	
	end function OpenGrADS
	
	
	function CloseGrADS(this, iLUN_D) result(iRetCode)
	
		! Routine arguments
		class(CalmetType), intent(in)	:: this
		integer, intent(in)				:: iLUN_D
		integer							:: iRetCode
		
		! Locals
		! -none-
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Close file
		close(iLUN_D)
		
	end function CloseGrADS
	
	
	function WriteGrADS(this, iLUN_D) result(iRetCode)
	
		! Routine arguments
		class(CalmetType), intent(inout)	:: this
		integer, intent(in)					:: iLUN_D
		integer								:: iRetCode
		
		! Locals
		integer	:: i
		integer	:: j
		integer	:: k
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write data in GrADS binary format
		do k = 1, this % iNz
			write(iLUN_D) ((this % raU(i,j,k),i=1,this%iNx),j=1,this%iNy)
		end do
		do k = 1, this % iNz
			write(iLUN_D) ((this % raV(i,j,k),i=1,this%iNx),j=1,this%iNy)
		end do
		do k = 1, this % iNz
			write(iLUN_D) ((this % raW(i,j,k),i=1,this%iNx),j=1,this%iNy)
		end do
		do k = 1, this % iNz
			write(iLUN_D) ((this % raZtemp(i,j,k),i=1,this%iNx),j=1,this%iNy)
		end do
		
	end function WriteGrADS
	
	
	function WriteDataToCalpuffIsc( &
		iLUN, sCalpuffIsc, &
		iTimeZone, iDeltaTime, rZ0, &
		ivTimeStamp, &
		rvVel, rvDir, &
		rvTemp, ivUrel, &
		rvRg, &
		rvRain, &
		ivPrecCode, &
		rvUstar, rvL, &
		rvZi, &
		ivIstab &
	) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sCalpuffIsc
		integer, intent(in)					:: iTimeZone
		integer, intent(in)					:: iDeltaTime
		real, intent(in)					:: rZ0
		integer, dimension(:), intent(in)	:: ivTimeStamp
		real, dimension(:), intent(in)		:: rvVel
		real, dimension(:), intent(in)		:: rvDir
		real, dimension(:), intent(in)		:: rvTemp
		integer, dimension(:), intent(in)	:: ivUrel
		real, dimension(:), intent(in)		:: rvRg
		real, dimension(:), intent(in)		:: rvRain
		integer, dimension(:), intent(in)	:: ivPrecCode
		real, dimension(:), intent(in)		:: rvUstar
		real, dimension(:), intent(in)		:: rvL
		real, dimension(:), intent(in)		:: rvZi
		integer, dimension(:), intent(in)	:: ivIstab
		integer								:: iRetCode
		
		! Locals
		integer	:: i
		real	:: rDir
		real	:: rHLM, rL
		real	:: rZi
		real	:: rTa
		integer	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer	:: iYearFrom, iYearTo
		integer	:: iPrecCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write data to file
		open(iLUN, file=sCalpuffIsc, status='unknown', action='write')
		! -1- Header
		call UnpackTime(ivTimeStamp(1), iYearFrom, iMonth, iDay, iHour, iMinute, iSecond)
		call UnpackTime(ivTimeStamp(SIZE(ivTimeStamp)-1), iYearTo, iMonth, iDay, iHour, iMinute, iSecond)
		if(iDeltaTime >= 3600) then
			write(iLUN,"(4(1x,i11))") 1, MOD(iYearFrom, 100), 1, MOD(iYearTo, 100)
		else
			write(iLUN,"('   1')")
			write(iLUN,"('Prepared by MetDecode')")
			write(iLUN,"('NONE')")
			if(iTimeZone > 0) then
				write(iLUN, "('UTC+',i2.2)") iTimeZone
			else
				write(iLUN, "('UTC',i3.3)") iTimeZone
			end if
			write(iLUN,"(4(1x,i11))") 1, MOD(iYearFrom, 100), 1, MOD(iYearTo, 100)
		end if
		
		do i = 1, SIZE(ivTimeStamp)
		
			! Prepare date and time, and change hour to reflect CALPUFF/ISC convention
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			iHour = iHour + 1
			
			! Print data the CALPUFF/ISC way
			if(iDeltaTime >= 3600) then
				write(iLUN,"(4i2,2f9.4,f6.1,i2,2f7.1,f9.4,f10.1,f8.4,i4,f7.2,a10,a5,1x,f8.1,i3)") &
					MOD(iYear, 100), iMonth, iDay, iHour, &
					rvDir(i), rvVel(i), rvTemp(i), ivIstab(i), rvZi(i), rvZi(i), &
					rvUstar(i), rvL(i), rZ0, ivPrecCode(i), rvRain(i), ' ', '     ', &
					rvRg(i), ivUrel(i)
			else
				write(iLUN,"(2(4i2,i4),2f9.4,f6.1,i2,2f7.1,f9.4,f10.1,f8.4,i4,f7.2,a10,a5,1x,f8.1,i3)") &
					MOD(iYear, 100), iMonth, iDay, iHour, iSecond, &
					MOD(iYear, 100), iMonth, iDay, iHour, iSecond + iDeltaTime, &
					rvDir(i), rvVel(i), rvTemp(i), ivIstab(i), rvZi(i), rvZi(i), &
					rvUstar(i), rvL(i), rZ0, ivPrecCode(i), rvRain(i), ' ', '     ', &
					rvRg(i), ivUrel(i)
			end if
            
		end do
		close(iLUN)

	end function WriteDataToCalpuffIsc
	

	function WriteDataToCalpuffCtdm( &
		iLUN_S, iLUN_P, &
		sSurfaceFile, sProfileFile, &
		sPmap, sTimeZone, &
		iDeltaTime, rZ0, rZr, &
		rvZ, &
		ivTimeStamp, &
		ivUrel, &
		rvRain, ivPrecCode, &
		rvRg, &
		rvUstar, &
		rvL, &
		rvZi, &
		ivIstab, &
		rmU, rmV, rmTemp &
	) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)					:: iLUN_S
		integer, intent(in)					:: iLUN_P
		character(len=*), intent(in)		:: sSurfaceFile
		character(len=*), intent(in)		:: sProfileFile
		character(len=*), intent(in)		:: sPmap
		character(len=*), intent(in)		:: sTimeZone
		integer, intent(in)					:: iDeltaTime
		real, intent(in)					:: rZ0
		real, intent(in)					:: rZr
		real, dimension(:), intent(in)		:: rvZ
		integer, dimension(:), intent(in)	:: ivTimeStamp
		integer, dimension(:), intent(in)	:: ivUrel
		integer, dimension(:), intent(in)	:: ivIstab
		real, dimension(:), intent(in)		:: rvZi
		real, dimension(:), intent(in)		:: rvUstar
		real, dimension(:), intent(in)		:: rvL
		real, dimension(:), intent(in)		:: rvRain
		integer, dimension(:), intent(in)	:: ivPrecCode
		real, dimension(:), intent(in)		:: rvRg
		real, dimension(:,:), intent(in)	:: rmU
		real, dimension(:,:), intent(in)	:: rmV
		real, dimension(:,:), intent(in)	:: rmTemp
		integer								:: iRetCode
		
		! Locals
		integer				:: i, j, k, n
		integer				:: iLast
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer				:: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer				:: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
		integer				:: iJday
		integer				:: iPrecCode
		real				:: rNewVel, rNewDir, rNewTemp
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Set initial and final times
		n = size(ivTimeStamp)
		call UnpackTime( &
			ivTimeStamp(1), &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom &
		)
		call UnpackTime( &
			ivTimeStamp(n) + iDeltaTime, &
			iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo &
		)
		
      	! Write surface data in CTDM/Calpuff6 form
      	open(iLUN_S, file=sSurfaceFile, status='unknown', action='write')
      	if(iDeltaTime < 3600) then
      	
			! Write header
			write(iLUN_S, "(2a16,a64)") &
				'SURFACE.DAT', '2.1', &
				'Header comments, begin/end times with seconds, time zone'
			write(iLUN_S, "('   1')")
			write(iLUN_S, "('Prepared by MetDecode')")
			write(iLUN_S, "(a)") sPmap
			write(iLUN_S, "(a)") sTimeZone
			write(iLUN_S, *) iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom*60+iSecondFrom
			
			! Write actual data
			do i = 1, size(ivTimeStamp)
				call UnpackTime( &
					ivTimeStamp(i), &
					iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom &
				)
				call UnpackTime( &
					ivTimeStamp(i) + iDeltaTime, &
					iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo &
				)
				write(iLUN_S, *) &
					iYearFrom, iMonthFrom, iDayFrom, &
					DayInYear(iYearFrom, iMonthFrom, iDayFrom), &
					iHourFrom, iMinuteFrom*60+iSecondFrom, &
					iYearTo, iMonthTo, iDayTo, &
					DayInYear(iYearTo, iMonthTo, iDayTo), &
					iHourTo, iMinuteTo*60+iSecondTo, &
					rvZi(i), rvZi(i), rvUstar(i), rvL(i), rZ0, &
					ivPrecCode(i), rvRain(i), rvRg(i), ivUrel(i)
			end do
			
      	else
      	
			! Write data
			do i = 1, size(ivTimeStamp)
				call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
				iJday = DayInYear(iYear, iMonth, iDay)
				write(iLUN_S,*) &
					MOD(iYear,100), iMonth, iDay, iJday, iHour+1, &
					INT(rvZi(i)), INT(rvZi(i)), &
					MAX(rvUstar(i), 0.001), rvL(i), &
					rZ0, ivPrecCode(i), rvRain(i), rvRg(i), ivUrel(i)
			end do
			
		end if
		close(iLUN_S)
      	
      	! Write profile data in CTDM/Calpuff6 form
		open(iLUN_P, file=sProfileFile, status='unknown', action='write')
      	if(iDeltaTime < 3600) then
      	
			! Write header
			write(iLUN_P, "(2a16,a64)") &
				'PROFILE.DAT', '2.1', &
				'Header comments, begin/end times with seconds, time zone'
			write(iLUN_P, "('   1')")
			write(iLUN_P, "('Prepared by MetDecode')")
			write(iLUN_P, "(a)") sPmap
			write(iLUN_P, "(a)") sTimeZone
			write(iLUN_P, *) iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom*60+iSecondFrom
			
			! Write actual data
			do i = 1, size(ivTimeStamp)
				call UnpackTime( &
					ivTimeStamp(i), &
					iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom &
				)
				call UnpackTime( &
					ivTimeStamp(i) + iDeltaTime, &
					iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo &
				)
				n = SIZE(rvZ)
				do j = 1, n-1
					iLast = 0
					if(j==n-1) iLast = 1
					rNewVel = SQRT(rmU(i,j)**2 + rmV(i,j)**2)
					if(rNewVel > 0.) then
						rNewDir = DIR_WIND(-rmU(i,j),-rmV(i,j))
					else
						rNewDir = 0.
					end if
					rNewTemp = rmTemp(i,j)
					write(iLUN_P, *) &
						iYearFrom, iMonthFrom, iDayFrom, DayInYear(iYearFrom, iMonthFrom, iDayFrom), &
						iHourFrom, iMinuteFrom*60 + iSecondFrom, &
						iYearTo, iMonthTo, iDayTo, DayInYear(iYearTo, iMonthTo, iDayTo), &
						iHourTo, iMinuteTo*60 + iSecondTo, &
						(rvZ(j) + rvZ(j+1))/2., iLast, rNewDir, rNewVel, rNewTemp, &
						-99.9, -9.90, -999.9, -99.9
						
				end do
			end do
			
      	else
      	
			! Write data
			n = SIZE(rvZ)
			do i = 1, size(ivTimeStamp)
				call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
				iJday = DayInYear(iYear, iMonth, iDay)
				do j = 1, n-1
					iLast = 0
					if(j==n-1) iLast = 1
					rNewVel = SQRT(rmU(i,j)**2 + rmV(i,j)**2)
					if(rNewVel > 0.) then
						rNewDir = DIR_WIND(-rmU(i,j),-rmV(i,j))
					else
						rNewDir = 0.
					end if
					rNewTemp = rmTemp(i,j)
					write(iLUN_P,*) &
						MOD(iYear,100), iMonth, iDay, iHour+1, &
						(rvZ(j)+rvZ(j+1))/2., iLast, rNewDir, rNewVel, rNewTemp, -99.9, -9.90, -999.9
				end do
			end do
			
		end if
		close(iLUN_P)

	end function WriteDataToCalpuffCtdm
	

	function WriteDataToCaline( &
		iLUN_S, &
		sSurfaceFile, &
		iDeltaTime, rZ0, rZr, &
		ivTimeStamp, &
		rvL, &
		rvZi, &
		ivIstab, &
		rmU, rmV &
	) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)					:: iLUN_S
		character(len=*), intent(in)		:: sSurfaceFile
		integer, intent(in)					:: iDeltaTime
		real, intent(in)					:: rZ0
		real, intent(in)					:: rZr
		integer, dimension(:), intent(in)	:: ivTimeStamp
		integer, dimension(:), intent(in)	:: ivIstab
		real, dimension(:), intent(in)		:: rvZi
		real, dimension(:), intent(in)		:: rvL
		real, dimension(:,:), intent(in)	:: rmU
		real, dimension(:,:), intent(in)	:: rmV
		integer								:: iRetCode
		
		! Locals
		integer				:: i, j, k, n
		integer				:: iLast
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer				:: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer				:: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
		integer				:: iJday
		integer				:: iPrecCode
		real				:: rNewVel, rNewDir, rNewTemp
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Set initial and final times
		n = size(ivTimeStamp)
		call UnpackTime( &
			ivTimeStamp(1), &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom &
		)
		call UnpackTime( &
			ivTimeStamp(n) + iDeltaTime, &
			iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo &
		)
		
      	! Write surface data in Caline form
      	if(iDeltaTime < 3600) then
      	
			! Do nothing: Caline only supports hourly time stamps
			iRetCode = 1
			
      	else
      	
			! Write actual data
			open(iLUN_S, file=sSurfaceFile, status='unknown', action='write')
			do i = 1, size(ivTimeStamp)
				call UnpackTime( &
					ivTimeStamp(i), &
					iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom &
				)
				call UnpackTime( &
					ivTimeStamp(i) + iDeltaTime, &
					iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo &
				)
				rNewVel = SQRT(((rmU(i,1)+rmU(i,2))/2.)**2 + ((rmV(i,1)+rmV(i,2))/2.)**2)
				if(rNewVel > 0.) then
					rNewDir = DIR_WIND(-(rmU(i,1)+rmU(i,2))/2.,-(rmV(i,1)+rmV(i,2))/2.)
				else
					rNewDir = 0.
				end if
				write(iLUN_S, "(i8,3i3,2f9.1,i3,f7.1,'      0. ''N'' 10 0 36')") &
					iYearFrom, iMonthFrom, iDayFrom, &
					iHourFrom, &
					rNewVel, rNewDir, ivIstab(i), rvZi(i)
			end do
			close(iLUN_S)
			
		end if
      	
	end function WriteDataToCaline
	

	function WriteDataToStandardProfile( &
		iLUN_P, &
		sProfileFile, &
		sPmap, sTimeZone, &
		iDeltaTime, rZ0, rZr, &
		rvZ, &
		ivTimeStamp, &
		ivUrel, &
		rvRain, ivPrecCode, &
		rvRg, &
		rvUstar, &
		rvL, &
		rvZi, &
		ivIstab, &
		rmU, rmV, rmW, rmTemp &
	) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)					:: iLUN_P
		character(len=*), intent(in)		:: sProfileFile
		character(len=*), intent(in)		:: sPmap
		character(len=*), intent(in)		:: sTimeZone
		integer, intent(in)					:: iDeltaTime
		real, intent(in)					:: rZ0
		real, intent(in)					:: rZr
		real, dimension(:), intent(in)		:: rvZ
		integer, dimension(:), intent(in)	:: ivTimeStamp
		integer, dimension(:), intent(in)	:: ivUrel
		integer, dimension(:), intent(in)	:: ivIstab
		real, dimension(:), intent(in)		:: rvZi
		real, dimension(:), intent(in)		:: rvUstar
		real, dimension(:), intent(in)		:: rvL
		real, dimension(:), intent(in)		:: rvRain
		integer, dimension(:), intent(in)	:: ivPrecCode
		real, dimension(:), intent(in)		:: rvRg
		real, dimension(:,:), intent(in)	:: rmU
		real, dimension(:,:), intent(in)	:: rmV
		real, dimension(:,:), intent(in)	:: rmW
		real, dimension(:,:), intent(in)	:: rmTemp
		integer								:: iRetCode
		
		! Locals
		integer				:: i, j, k, n
		integer				:: iLast
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		integer				:: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
		integer				:: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
		integer				:: iJday
		integer				:: iPrecCode
		real				:: rNewVel, rNewDir, rNewTemp
		real				:: rNewZ
		character(len=8)	:: sInPbl
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Set initial and final times
		n = size(ivTimeStamp)
		call UnpackTime( &
			ivTimeStamp(1), &
			iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom &
		)
		call UnpackTime( &
			ivTimeStamp(n) + iDeltaTime, &
			iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo &
		)
		
      	! Write profile data in CTDM/Calpuff6 form
		open(iLUN_P, file=sProfileFile, status='unknown', action='write')
		
		! Write actual data
		do i = 1, size(ivTimeStamp)
			call UnpackTime( &
				ivTimeStamp(i), &
				iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom &
			)
			write(iLUN_P, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
				iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
			write(iLUN_P,"('In.PBL?,Z,U,V,W,Dir,Vel,Temp')")
			n = SIZE(rvZ)
			do j = 1, n-1
				rNewVel = SQRT(rmU(i,j)**2 + rmV(i,j)**2)
				if(rNewVel > 0.) then
					rNewDir = DIR_WIND(-rmU(i,j),-rmV(i,j))
				else
					rNewDir = 0.
				end if
				rNewTemp = rmTemp(i,j) - 273.15
				rNewZ = (rvZ(j) + rvZ(j+1))/2.
				if(rNewZ <= rvZi(i)) then
					sInPbl = "PBL"
				else
					sInPbl = "Free Atm"
				end if
				write(iLUN_P, "(a,7(',',f7.2))") &
					sInPbl, rNewZ, rmU(i,j), rmV(i,j), rmW(i,j), rNewDir, rNewVel, rNewTemp
			end do
			write(iLUN_P,"(1x)")
			
		end do
		
		close(iLUN_P)

	end function WriteDataToStandardProfile
	
	
	subroutine cleanCalpuffHeader(this)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		
		! Locals
		! -none-
		
		! Reclaim allocated space, if any
		if(allocated(this % svCommentLine)) deallocate(this % svCommentLine)
		if(allocated(this % svSpeciesName)) deallocate(this % svSpeciesName)
		if(allocated(this % svSpeciesUnit)) deallocate(this % svSpeciesUnit)
		if(allocated(this % rvFreeX)) deallocate(this % rvFreeX)
		if(allocated(this % rvFreeY)) deallocate(this % rvFreeY)
		if(allocated(this % rvFreeZ)) deallocate(this % rvFreeZ)
		if(allocated(this % rvFreeHeight)) deallocate(this % rvFreeHeight)
		if(allocated(this % ivFreeGroupIdx)) deallocate(this % ivFreeGroupIdx)
		if(allocated(this % svFreeGroup)) deallocate(this % svFreeGroup)
		if(allocated(this % rvFreeConc)) deallocate(this % rvFreeConc)
		if(allocated(this % rvFreeCompressedConc)) deallocate(this % rvFreeCompressedConc)
		if(allocated(this % rvCTSGX)) deallocate(this % rvCTSGX)
		if(allocated(this % rvCTSGY)) deallocate(this % rvCTSGY)
		if(allocated(this % rvCTSGZ)) deallocate(this % rvCTSGZ)
		if(allocated(this % ivHill)) deallocate(this % ivHill)
		if(allocated(this % rvCTSGConc)) deallocate(this % rvCTSGConc)
		if(allocated(this % rvCTSGCompressedConc)) deallocate(this % rvCTSGCompressedConc)
		if(allocated(this % rmConc)) deallocate(this % rmConc)
		if(allocated(this % imExceedances)) deallocate(this % imExceedances)
		if(allocated(this % rmSumConc)) deallocate(this % rmSumConc)
		if(allocated(this % rmSum2Conc)) deallocate(this % rmSum2Conc)
		if(allocated(this % rmAvgConc)) deallocate(this % rmAvgConc)
		if(allocated(this % rmStdConc)) deallocate(this % rmStdConc)
		if(allocated(this % rmMaxConc)) deallocate(this % rmMaxConc)
		if(allocated(this % imMaxPos)) deallocate(this % imMaxPos)
		if(allocated(this % rvConc)) deallocate(this % rvConc)
		if(allocated(this % rvCompressedData)) deallocate(this % rvCompressedData)
		if(allocated(this % svSeriesFile)) deallocate(this % svSeriesFile)
		if(allocated(this % svPointSource1)) deallocate(this % svPointSource1)
		if(allocated(this % svPointSource2)) deallocate(this % svPointSource2)
		if(allocated(this % svAreaSource1)) deallocate(this % svAreaSource1)
		if(allocated(this % svAreaSource2)) deallocate(this % svAreaSource2)
		if(allocated(this % svLineSource1)) deallocate(this % svLineSource1)
		if(allocated(this % svLineSource2)) deallocate(this % svLineSource2)
		if(allocated(this % svVolumeSource1)) deallocate(this % svVolumeSource1)
		if(allocated(this % svVolumeSource2)) deallocate(this % svVolumeSource2)
		if(allocated(this % smSourceName)) deallocate(this % smSourceName)
		if(allocated(this % ivNumSourcesPerType)) deallocate(this % ivNumSourcesPerType)
		
	end subroutine cleanCalpuffHeader
	
	
	! Get header from Calpuff concentration files. Currently, version 2.2 is supported for
	! Calpuff 6 (Calpuff 7 uses a different format altogether, which is supported as-is).
	function getCalpuffHeader(this, iLUN, sInputFile, iCpuffMode, sErrStr) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(out)	:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sInputFile
		integer, intent(in)				:: iCpuffMode
		character(len=128), intent(out)	:: sErrStr
		integer							:: iRetCode
		
		! Locals
		integer						:: iErrCode
		integer						:: iNumStrings
		integer						:: iString
		character(len=132)			:: sCommentLine
		character(len=15)			:: sSpecies
		integer						:: iSpecies
		integer						:: iGroup
		integer						:: iSource
		integer						:: iSourceType
		integer						:: iSourceTypeAsRead
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Connect input file, with minimal check on input variables
		open(iLUN, FILE=sInputFile, status='OLD', form='UNFORMATTED', action='READ', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 10
			sErrStr = "- Fatal - Input file not found"
			return
		end if
		
		! ************************************************************
		! * Header part, as in CALPOST's "GETHEAD()" and subroutines *
		! ************************************************************
		
		! Get first record, and check it corresponds to meaningful data
		read(iLUN, iostat=iErrCode) &
			this % sDataSet, &
			this % sDataVersion, &
			this % sDataModel
		if(iErrCode /= 0) then
			iRetCode = 11
			close(iLUN)
			sErrStr = "- Fatal - Record #1 (DataSet, DataVersion, DataModel) invalid"
			return
		end if
		if(this % sDataSet /= 'CONC.DAT' .AND. this % sDataSet /= 'DFLX.DAT' .AND. this % sDataSet /= 'WFLX.DAT') then
			iRetCode = 12
			close(iLUN)
			sErrStr = "- Fatal - Record #1 DataSet is not 'CONC.DAT', 'DFLX.DAT' or 'WFLX.DAT'"
			return
		end if
		! Post-condition: Now, data file type fits one of the three possibilities expected
		
		! Get the pre-header comment information, containing the
		! CALPUFF input file
		read(iLUN, iostat=iErrCode) iNumStrings
		if(iErrCode /= 0) then
			iRetCode = 13
			close(iLUN)
			sErrStr = "- Fatal - Comment block: invalid number of comments"
			return
		end if
		allocate(this % svCommentLine(iNumStrings))
		do iString = 1, iNumStrings
			read(iLUN, iostat=iErrCode) sCommentLine
			this % svCommentLine(iString) = sCommentLine
			if(iErrCode /= 0) then
				iRetCode = 14
				close(iLUN)
				sErrStr = "- Fatal - Comment block: invalid comment text"
				return
			end if
		end do
		
		! Read header record
		if(iCpuffMode == 6) then
			read(iLUN, iostat=iRetCode) &
				this % sModel, &
				this % sVersion, &
				this % sLevel, &
				this % iBeginYear, &
				this % iBeginJulday, &
				this % iBeginHour, &
				this % iBeginSecond, &
				this % sTimeZone, &
				this % iModelSteps, &
				this % iLoggingTime, &
				this % iAveragingTime, &
				this % iNx, &
				this % iNy, &
				this % rDx, &
				this % rDy, &
				this % iNumLevels, &
				this % rX0, &
				this % rY0, &
				this % iNumSurfaceStations, &
				this % iMinCompX, &
				this % iMaxCompX, &
				this % iMinCompY, &
				this % iMaxCompY, &
				this % iMinSampX, &
				this % iMinSampY, &
				this % iMaxSampX, &
				this % iMaxSampY, &
				this % iMeshFactor, &
				this % iNumPointSources1, &
				this % iNumPointSources2, &
				this % iNumAreaSources1, &
				this % iNumAreaSources2, &
				this % iNumLineSources1, &
				this % iNumLineSources2, &
				this % iNumVolumeSources1, &
				this % iNumVolumeSources2, &
				this % iMsource, &
				this % iNumFreeReceptors, &
				this % iNumCTSGReceptors, &
				this % lSamplingGrid, &
				this % iNumChemicalSpecies, &
				this % lCompressed, &
				this % i2Dmet, &
				this % iUTMZone, &
				this % rFalseEasting, &
				this % rFalseNorthing, &
				this % rNlat0, &
				this % rElon0, &
				this % rXlat1, &
				this % rXlat2, &
				this % sPmap, &
				this % sUTMHem, &
				this % sDatum, &
				this % sDaten, &
				this % sClat0, &
				this % sClon0, &
				this % sClat1, &
				this % sClat2
			if(iErrCode /= 0) then
				iErrCode = 15
				close(iLUN)
				sErrStr = "- Fatal - Invalid header"
				return
			end if
			if(this % iNumChemicalSpecies <= 0) then
				iRetCode = 16
				close(iLUN)
				sErrStr = "- Fatal - Number of chemical species modeled is <= 0"
				return
			end if
		else	! That is, iCpuffMode == 7, by construction
			read(iLUN, iostat=iRetCode) &
				this % sModel, &
				this % sVersion, &
				this % sLevel, &
				this % iBeginYear, &
				this % iBeginJulday, &
				this % iBeginHour, &
				this % iBeginSecond, &
				this % sTimeZone, &
				this % iModelSteps, &
				this % iLoggingTime, &
				this % iAveragingTime, &
				this % iNx, &
				this % iNy, &
				this % rDx, &
				this % rDy, &
				this % iNumLevels, &
				this % rX0, &
				this % rY0, &
				this % iNumSurfaceStations, &
				this % iMinCompX, &
				this % iMaxCompX, &
				this % iMinCompY, &
				this % iMaxCompY, &
				this % iMinSampX, &
				this % iMinSampY, &
				this % iMaxSampX, &
				this % iMaxSampY, &
				this % iMeshFactor, &
				this % iNumSourceTypes, &
				this % iMsource, &
				this % iNumFreeReceptors, &
				this % iNumReceptorGroups, &
				this % iNumCTSGReceptors, &
				this % lSamplingGrid, &
				this % iNumChemicalSpecies, &
				this % lCompressed, &
				this % i2Dmet, &
				this % iUTMZone, &
				this % rFalseEasting, &
				this % rFalseNorthing, &
				this % rNlat0, &
				this % rElon0, &
				this % rXlat1, &
				this % rXlat2, &
				this % sPmap, &
				this % sUTMHem, &
				this % sDatum, &
				this % sDaten, &
				this % sClat0, &
				this % sClon0, &
				this % sClat1, &
				this % sClat2
			if(iErrCode /= 0) then
				iErrCode = 15
				close(iLUN)
				sErrStr = "- Fatal - Invalid header"
				return
			end if
			if(this % iNumChemicalSpecies <= 0) then
				iRetCode = 16
				close(iLUN)
				sErrStr = "- Fatal - Number of chemical species modeled is <= 0"
				return
			end if
			if(this % iNumSourceTypes <= 0) then
				iRetCode = 16
				close(iLUN)
				sErrStr = "- Fatal - Number of pollutant sources is <= 0"
				return
			end if
			allocate(this % ivNumSourcesPerType(this % iNumSourceTypes))
			this % ivNumSourcesPerType = 0
			read(iLUN, iostat=iRetCode) &
				(this % ivNumSourcesPerType(iSourceType), iSourceType = 1, this % iNumSourceTypes)
			if(iRetCode /= 0 .or. this % iNumSourceTypes <= 0) then
				iRetCode = 16
				close(iLUN)
				sErrStr = "- Fatal - Invalid number of sources per type"
				return
			end if
		end if
		
		! Reserve workspace for chemical species and receptor data
		allocate(this % svSpeciesName(this % iNumChemicalSpecies))
		allocate(this % svSpeciesUnit(this % iNumChemicalSpecies))
		if(this % iNumFreeReceptors > 0) then
			allocate(this % rvFreeX(this % iNumFreeReceptors))
			allocate(this % rvFreeY(this % iNumFreeReceptors))
			allocate(this % rvFreeZ(this % iNumFreeReceptors))
			allocate(this % rvFreeConc(this % iNumFreeReceptors, this % iNumChemicalSpecies))
			allocate(this % rvFreeCompressedConc(this % iNumFreeReceptors))
			if(iCpuffMode == 7) then
				allocate(this % rvFreeHeight(this % iNumFreeReceptors))
				allocate(this % ivFreeGroupIdx(this % iNumFreeReceptors))
			end if
		end if
		if(this % iNumCTSGReceptors > 0) then
			allocate(this % rvCTSGX(this % iNumCTSGReceptors))
			allocate(this % rvCTSGY(this % iNumCTSGReceptors))
			allocate(this % rvCTSGZ(this % iNumCTSGReceptors))
			allocate(this % ivHill(this % iNumCTSGReceptors))
			allocate(this % rvCTSGConc(this % iNumCTSGReceptors, this % iNumChemicalSpecies))
			allocate(this % rvCTSGCompressedConc(this % iNumCTSGReceptors))
		end if
		
		! Computes dimension of the sampling grid, and then reserve
		! workspace based on the information gathered
		if(this % lSamplingGrid) then
			this % iNxSampling = this % iMeshFactor * (this % iMaxSampX - this % iMinSampX) + 1
			this % iNySampling = this % iMeshFactor * (this % iMaxSampY - this % iMinSampY) + 1
		else
			this % iNxSampling = 0
			this % iNySampling = 0
		end if
		
		if(this % iNxSampling>0 .AND. this % iNySampling>0) then
			allocate(this % imExceedances(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % rmConc(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % rmSumConc(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % rmSum2Conc(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % rmAvgConc(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % rmStdConc(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % rmMaxConc(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % imMaxPos(this % iNxSampling, this % iNySampling, this % iNumChemicalSpecies))
			allocate(this % rvConc(this % iNxSampling * this % iNySampling))
			allocate(this % rvCompressedData(this % iNxSampling * this % iNySampling))
			allocate(this % svSeriesFile(this % iNumChemicalSpecies))
		end if
		
		! Get run title
		read(iLUN, iostat=iErrCode) this % svTitle
		if(iErrCode /= 0) then
			iRetCode = 17
			close(iLUN)
			sErrStr = "- Fatal - Invalid run title"
			return
		end if
		
		! Get chemical species
		if(iCpuffMode == 6) then
			read(iLUN,iostat=iErrCode) (this % svSpeciesName(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
			if(iErrCode /= 0) then
				iRetCode = 18
				sErrStr = "- Fatal - Invalid chemical specie name(s)"
				close(iLUN)
				return
			end if
			do iSpecies = 1, this % iNumChemicalSpecies
				this % svSpeciesName(iSpecies) = this % svSpeciesName(iSpecies)(1:12)	! Ignore layer no. on chars 13:15 (always 1 in CALPUFF, usually > 1 in CALGRID)
			end do
			read(iLUN,iostat=iErrCode) (this % svSpeciesUnit(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
			if(iErrCode /= 0) then
				iRetCode = 19
				sErrStr = "- Fatal - Invalid chemical specie unit(s)"
				close(iLUN)
				return
			end if
		else
			read(iLUN,iostat=iErrCode) (this % svSpeciesName(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
			if(iErrCode /= 0) then
				iRetCode = 18
				sErrStr = "- Fatal - Invalid chemical specie name(s)"
				close(iLUN)
				return
			end if
			do iSpecies = 1, this % iNumChemicalSpecies
				this % svSpeciesName(iSpecies) = this % svSpeciesName(iSpecies)(1:12)	! Ignore layer no. on chars 13:15 (always 1 in CALPUFF, usually > 1 in CALGRID)
			end do
			read(iLUN,iostat=iErrCode) (this % svSpeciesUnit(iSpecies), iSpecies = 1, this % iNumChemicalSpecies)
			if(iErrCode /= 0) then
				iRetCode = 19
				sErrStr = "- Fatal - Invalid chemical specie unit(s)"
				close(iLUN)
				return
			end if
		end if
		
		! Get non-gridded receptors, if any
		if(this % iNumFreeReceptors > 0) then
			if(iCpuffMode == 6) then
				read(iLUN, iostat=iErrCode) &
					this % rvFreeX, &
					this % rvFreeY, &
					this % rvFreeZ
				if(iErrCode /= 0) then
					iRetCode = 20
					close(iLUN)
					sErrStr = "- Fatal - Invalid free receptors data"
					return
				end if
			else
				read(iLUN, iostat=iErrCode) &
					this % rvFreeX, &
					this % rvFreeY, &
					this % rvFreeZ, &
					this % rvFreeHeight, &
					this % ivFreeGroupIdx
				if(iErrCode /= 0) then
					iRetCode = 20
					close(iLUN)
					sErrStr = "- Fatal - Invalid free receptors data"
					return
				end if
				read(iLUN, iostat=iErrCode) &
					(this % svFreeGroup(iGroup), iGroup = 1, this % iNumReceptorGroups)
				if(iErrCode /= 0) then
					iRetCode = 20
					close(iLUN)
					sErrStr = "- Fatal - Invalid free receptors group names"
					return
				end if
			end if
		end if
		
		! Get complex-terrain receptors, if any
		if(this % iNumCTSGReceptors > 0) then
			read(iLUN, iostat=iErrCode) &
				this % rvFreeX, &
				this % rvFreeY, &
				this % rvFreeZ, &
				this % ivHill
			if(iErrCode /= 0) then
				iRetCode = 21
				close(iLUN)
				sErrStr = "- Fatal - Invalid complex-terrain receptors"
				return
			end if
		end if
		
		! Get source names
		if(iCpuffMode == 6) then
			if(this % iNumPointSources1 > 0) then
				allocate(this % svPointSource1(this % iNumPointSources1))
				read(iLUN, iostat=iErrCode) iSourceType, this % svPointSource1
				if(iErrCode /= 0) then
					iRetCode = 22
					close(iLUN)
					sErrStr = "- Fatal - Error reading point sources 1"
					return
				end if
			end if
			if(this % iNumPointSources2 > 0) then
				allocate(this % svPointSource2(this % iNumPointSources2))
				read(iLUN, iostat=iErrCode) iSourceType, this % svPointSource2
				if(iErrCode /= 0) then
					iRetCode = 23
					close(iLUN)
					sErrStr = "- Fatal - Error reading point sources 2"
					return
				end if
			end if
			if(this % iNumAreaSources1 > 0) then
				allocate(this % svAreaSource1(this % iNumAreaSources1))
				read(iLUN, iostat=iErrCode) iSourceType, this % svAreaSource1
				if(iErrCode /= 0) then
					iRetCode = 24
					close(iLUN)
					sErrStr = "- Fatal - Error reading area sources 1"
					return
				end if
			end if
			if(this % iNumAreaSources2 > 0) then
				allocate(this % svAreaSource2(this % iNumAreaSources2))
				read(iLUN, iostat=iErrCode) iSourceType, this % svAreaSource2
				if(iErrCode /= 0) then
					iRetCode = 25
					close(iLUN)
					sErrStr = "- Fatal - Error reading area sources 2"
					return
				end if
			end if
			if(this % iNumLineSources1 > 0) then
				allocate(this % svLineSource1(this % iNumLineSources1))
				read(iLUN, iostat=iErrCode) iSourceType, this % svLineSource1
				if(iErrCode /= 0) then
					iRetCode = 26
					close(iLUN)
					sErrStr = "- Fatal - Error reading line sources 1"
					return
				end if
			end if
			if(this % iNumLineSources2 > 0) then
				allocate(this % svLineSource2(this % iNumLineSources2))
				read(iLUN, iostat=iErrCode) iSourceType, this % svLineSource2
				if(iErrCode /= 0) then
					iRetCode = 27
					close(iLUN)
					sErrStr = "- Fatal - Error reading line sources 2"
					return
				end if
			end if
			if(this % iNumVolumeSources1 > 0) then
				allocate(this % svVolumeSource1(this % iNumVolumeSources1))
				read(iLUN, iostat=iErrCode) iSourceType, this % svVolumeSource1
				if(iErrCode /= 0) then
					iRetCode = 28
					close(iLUN)
					sErrStr = "- Fatal - Error reading volume sources 1"
					return
				end if
			end if
			if(this % iNumVolumeSources2 > 0) then
				allocate(this % svVolumeSource2(this % iNumVolumeSources2))
				read(iLUN, iostat=iErrCode) iSourceType, this % svVolumeSource2
				if(iErrCode /= 0) then
					iRetCode = 29
					close(iLUN)
					sErrStr = "- Fatal - Error reading volume sources 2"
					return
				end if
			end if
		else
			allocate(this % smSourceName(maxval(this % ivNumSourcesPerType), this % iNumSourceTypes))
			do iSourceType = 1, this % iNumSourceTypes
				if(this % ivNumSourcesPerType(iSourceType) > 0) then
					read(iLUN, iostat=iErrCode) iSourceTypeAsRead, &
						(this % smSourceName(iSource, iSourceTypeAsRead), &
							iSource = 1, this % ivNumSourcesPerType(iSourceTypeAsRead))
				end if
				if(iErrCode /= 0) then
					iRetCode = 30
					close(iLUN)
					sErrStr = "- Fatal - Error reading source names"
					return
				end if
			end do		
		end if
		this % lHasBeenRead  = .false.
		this % imExceedances = 0
		this % iNumConc      = 0
		
		! Assign metadata
		this % iCpuffMode = iCpuffMode

	end function getCalpuffHeader


	subroutine cleanCalpuffRecord(this)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		
		! Locals
		! -none-
		
		! Do nothing: all useful work is made during header clean-up
		
	end subroutine cleanCalpuffRecord
	

	function getCalpuffRecord(this, iLUN, tOpts, lARM2) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		logical, intent(in), optional			:: lARM2
		integer									:: iRetCode
		
		! Locals
		integer											:: iErrCode
		integer											:: iSpecies
		integer											:: iNumExpectedData
		integer											:: iNumActualWords
		character(len=15)                   			:: sSpeciesName
		integer                             			:: iPos
		integer                             			:: iData
		integer                             			:: iX
		integer                             			:: iY
		integer                             			:: iZero
		integer                             			:: iNumZeros
		integer, dimension(2)                           :: ivPos
		real											:: rFactor
		real											:: rFactorConversion
		logical											:: lIsARM2
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize, based on options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		
		! Check ARM2 estimation for NO2 starting from NOx is to be used
		lIsARM2 = .false.
		if(present(lARM2)) then
			lIsARM2 = lARM2
		end if
		
        ! Get a new data record header, if any
        read(iLUN, iostat=iErrCode) &
			this % iYear, &
			this % iJulday, &
			this % iHour, &
			this % iSecond, &
            this % iYear2, &
            this % iJulday2, &
            this % iHour2, &
            this % iSecond2
        if(iErrCode /= 0) then
			iRetCode = -1	! Success, with end-of-file (special case)
			close(iLUN)
			return
        end if
		call ExpandDate( &
			this % iYear, this % iJulday, this % iHour, this % iSecond, &
			this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
			this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay &
		)
        
        ! Get source info
        read(iLUN, iostat=iErrCode) &
			this % iSrcType, &
			this % iSrcNum, &
			this % sSrcName, &
			this % rSrcX, &
			this % rSrcY
        if(iErrCode /= 0) then
			iRetCode = 50
			close(iLUN)
			return
        end if
        
        ! Get concentration for each species
        do iSpecies = 1, this % iNumChemicalSpecies
        
            ! Get gridded concentrations, if any
            if(this % lSamplingGrid) then
            
                ! Read data, using compressed or lazy form, as required
                if(this % lCompressed) then
                
                    ! Get expected and actual numbers of data: these are used
                    ! by the compression scheme used for CALPUFF data
                    iNumExpectedData = this % iNxSampling * this % iNySampling
                    read(iLUN, iostat=iErrCode) iNumActualWords
                    if(iErrCode /= 0) then
						iRetCode = 51
						close(iLUN)
						return
                    end if
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvCompressedData(1:iNumActualWords)
                    if(iErrCode /= 0) then
						iRetCode = 52
						close(iLUN)
						return
                    end if
                    iPos = 0
                    this % rvConc = 0.
                    do iData = 1, iNumActualWords
                        if(this % rvCompressedData(iData) > 0.) then
                            iPos = iPos + 1
                            this % rvConc(iPos) = this % rvCompressedData(iData)
                        else
                            iNumZeros = ABS(this % rvCompressedData(iData))
                            do iZero = 1, iNumZeros
                                iPos = iPos + 1
                                this % rvConc(iPos) = 0.
                            end do
                        end if
                    end do
                    if(iPos > iNumExpectedData) then
						iRetCode = 53
						close(iLUN)
						return
                    end if
                    
                else
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvConc
                    if(iErrCode /= 0) then
						iRetCode = 54
						close(iLUN)
						return
                    end if
                    
                end if
                
                ! Apply ARM2 correction, if requested
                if(lIsARM2) call correct_arm2(this % rvConc)
                
                if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
                
	                ! Transfer data from linear vector to final array
	                iPos = 0
	                do iY = 1, this % iNySampling
	                    do iX = 1, this % iNxSampling
	                        iPos = iPos + 1
	                        this % rmConc(iX, iY, iSpecies) = this % rvConc(iPos)
	                    end do
	                end do
                
					! Accumulate statistics for total concentration
					where(this % rmConc(:,:,iSpecies) > this % rmMaxConc(:,:,iSpecies))
						this % rmMaxConc(:,:,iSpecies) = this % rmConc(:,:,iSpecies)
					end where
					ivPos = MAXLOC(this % rmConc(:,:,iSpecies))
					if(maxval(this % rmConc(:,:,iSpecies)) > 0.) then
						this % imMaxPos(ivPos(1),ivPos(2),iSpecies) = this % imMaxPos(ivPos(1),ivPos(2),iSpecies) + 1
					end if
					this % rmSumConc(:,:,iSpecies)  = this % rmSumConc(:,:,iSpecies) + this % rmConc(:,:,iSpecies)
					this % rmSum2Conc(:,:,iSpecies) = this % rmSum2Conc(:,:,iSpecies) + this % rmConc(:,:,iSpecies)**2
					this % iNumConc = this % iNumConc + 1
                
	            end if
            
                ! Roughly inform user of current processing
                write(*,"('GRID,',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7,',',i3,',',i3,',',a)") &
					this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
					this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay, &
					MAXVAL(this % rmConc(:,:,iSpecies)), MAXLOC(this % rmConc(:,:,iSpecies)), TRIM(sSpeciesName(:12))
					
				! Add row to diagnostic series (computed only on gridded data)
				if(this % iSrcType == 0) then
					if(MAXVAL(this % rmConc) > 0.0) then
						this % rConcRatio = &
							MAXVAL(this % rmConc(:,:,iSpecies)) / &
							(SUM(this % rmConc(:,:,iSpecies))/SIZE(this % rmConc(:,:,iSpecies)))
						this % iNumNonzeroConc  = COUNT(this % rmConc(:,:,iSpecies) > 0.0)
						this % rMeanNonzeroConc = SUM(this % rmConc(:,:,iSpecies)) / this % iNumNonzeroConc
					end if
				end if
					
            end if
            
            ! Get concentration from non-gridded receptors
            if(this % iNumFreeReceptors > 0) then
            
                ! Read data, using compressed or lazy form, as required
                if(this % lCompressed) then
                
                    ! Get expected and actual numbers of data: these are used
                    ! by the compression scheme used for CALPUFF data
                    iNumExpectedData = this % iNumFreeReceptors
                    read(iLUN, iostat=iErrCode) iNumActualWords
                    if(iErrCode /= 0) then
						iRetCode = 55
						close(iLUN)
						return
                    end if
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvFreeCompressedConc(1:iNumActualWords)
                    if(iErrCode /= 0) then
						iRetCode = 56
						close(iLUN)
						return
                    end if
                    iPos = 0
                    do iData = 1, iNumActualWords
                        if(this % rvFreeCompressedConc(iData) >= 0.) then
                            iPos = iPos + 1
                            this % rvFreeConc(iPos,iSpecies) = this % rvFreeCompressedConc(iData)
                        else
                            iNumZeros = ABS(this % rvFreeCompressedConc(iData))
                            do iZero = 1, iNumZeros
                                iPos = iPos + 1
                                this % rvFreeConc(iPos,iSpecies) = 0.
                            end do
                        end if
                    end do
                    if(iPos /= iNumExpectedData) then
						iRetCode = 57
						close(iLUN)
						return
                    end if
                    
                else
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvFreeConc(:,iSpecies)
                    if(iErrCode /= 0) then
						iRetCode = 58
						close(iLUN)
						return
                    end if
                    
                end if
                
                ! Apply ARM2 correction, if requested
                if(lIsARM2) call correct_arm2(this % rvFreeConc(:,iSpecies))
                
                ! Inform user
                write(*,"('FREE,',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7,',',i3,',',i3,',',a)") &
					this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
					this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay, &
					MAXVAL(this % rvFreeConc(:,iSpecies)), MAXLOC(this % rvFreeConc(:,iSpecies)), -1, &
					TRIM(sSpeciesName(:12))
                
            end if
            
            ! Get concentrations from complex-terrain receptors
            if(this % iNumCTSGReceptors > 0) then
            
                ! Read data, using compressed or lazy form, as required
                if(this % lCompressed) then
                
                    ! Get expected and actual numbers of data: these are used
                    ! by the compression scheme used for CALPUFF data
                    iNumExpectedData = this % iNumFreeReceptors
                    read(iLUN, iostat=iErrCode) iNumActualWords
                    if(iErrCode /= 0) then
						iRetCode = 59
						close(iLUN)
						return
                    end if
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvCTSGCompressedConc(1:iNumActualWords)
                    if(iErrCode /= 0) then
						iRetCode = 60
						close(iLUN)
						return
                    end if
                    iPos = 0
                    do iData = 1, iNumActualWords
                        if(this % rvCTSGCompressedConc(iData) >= 0.) then
                            iPos = iPos + 1
                            this % rvCTSGConc(iPos,iSpecies) = this % rvCTSGCompressedConc(iData)
                        else
                            iNumZeros = ABS(this % rvCTSGCompressedConc(iData))
                            do iZero = 1, iNumZeros
                                iPos = iPos + 1
                                this % rvCTSGConc(iPos,iSpecies) = 0.
                            end do
                        end if
                    end do
                    if(iPos /= iNumExpectedData) then
						iRetCode = 61
						close(iLUN)
						return
                    end if
                    
                else
                    
                    ! Read species name and concentration data in compressed form,
                    ! then, if all is OK, decompress them to concentration array.
                    read(iLUN, iostat=iErrCode) sSpeciesName, this % rvCTSGConc(:,iSpecies)
                    if(iErrCode /= 0) then
						iRetCode = 62
						close(iLUN)
						return
                    end if
                    
                end if
                
                ! Apply ARM2 correction, if requested
                if(lIsARM2) call correct_arm2(this % rvCTSGConc(:,iSpecies))
                
                ! Inform user
                write(*,"('CTSG,',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',e15.7,',',i3,',',i3,',',a)") &
					this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
					this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay, &
					MAXVAL(this % rvCTSGConc(:,iSpecies)), MAXLOC(this % rvCTSGConc(:,iSpecies)), -1, &
					TRIM(sSpeciesName(:12))
                
            end if
        
        end do
        
        this % lHasBeenRead = .TRUE.
		
	end function getCalpuffRecord


	function writeCalpuffGridSpec(this, iLUN, sGridFile) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sGridFile
		integer								:: iRetCode
		
		! Locals
		! --none--
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write grid information in the form expected by ST post-processing chain
		open(iLUN, FILE=sGridFile, status='unknown', action='write')
		write(iLUN, "('creaMixAll_per10  ' 2(1x,i3),2(1x,f8.1),2(1x,f9.1) '  1. %3')") &
			this % iNxSampling, this % iNySampling, &
			1000. * this % rDx / this % iMeshFactor, &
			1000. * this % rDy / this % iMeshFactor, &
			1000. * (this % rX0+(this % iMinSampX-1) * this % rDx + 0.5 * this % rDx), &
			1000. * (this % rY0+(this % iMinSampY-1) * this % rDy + 0.5 * this % rDy)
		write(iLUN, *) this % iNumFreeReceptors, this % iNumCTSGReceptors
		write(iLUN, *) this % iNumChemicalSpecies
		close(iLUN)

	end function writeCalpuffGridSpec


	function writeCalpuffGrid(this, iLUN, sGridFile) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sGridFile
		integer								:: iRetCode
		
		! Locals
		integer	:: iX, iY
		real(8)	:: rDelta
		real(8)	:: rX0, rX
		real(8)	:: rY0, rY
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Compute actual grid parameters in meters (SW origin expressed at SW corner of SW cell)
		rDelta = 1000.d0 * this % rDx / this % iMeshFactor
		rX0    = 1000.d0 * (this % rX0 + this % rDx * (this % iMinSampX - 1) + this % rDx / 2.)
		rY0    = 1000.d0 * (this % rY0 + this % rDy * (this % iMinSampY - 1) + this % rDy / 2.)
		
		! Write grid information in the form expected by ST post-processing chain
		open(iLUN, FILE=sGridFile, status='unknown', action='write')
		write(iLUN, "('Easting, Northing')")
		do iY = 1, (this % iMaxSampY - this % iMinSampY) * this % iMeshFactor + 1
			rY = rY0 + (iY - 1) * rDelta
			do iX = 1, (this % iMaxSampX - this % iMinSampX) * this % iMeshFactor + 1
				rX = rX0 + (iX - 1) * rDelta
				write(iLUN, "(f10.2,',',f10.2)") rX, rY
			end do
		end do
		close(iLUN)

	end function writeCalpuffGrid


	function openCalpuff01(this, iLUN, sFile) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sFile
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write grid information in the form expected by ST post-processing chain
		open(iLUN, FILE=sFile, status='unknown', action='write', access='stream', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if

	end function openCalpuff01


	function writeCalpuff01(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iDesiredSpecies
		integer	:: iSpecies
		integer	:: iX
		integer	:: iY
		real	:: rFactor
		real	:: rFactorConversion
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize, based on options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		iDesiredSpecies   = tOpts % iSpeciesIdx
		
        ! Get concentration for each species
        do iSpecies = 1, this % iNumChemicalSpecies
        
			if(iSpecies == iDesiredSpecies) then
        
				! Get gridded concentrations, if any
				if(this % lSamplingGrid) then
				
					if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
					
						! Write data in ST01 form
						write(iLUN) &
							((this % rmConc(iX,iY,iSpecies) * rFactor * rFactorConversion, &
							iX=1, this % iNxSampling), &
							iY=1, this % iNySampling)
						
					end if
				
				end if
				
				! Get concentration from non-gridded receptors
				if(this % iNumFreeReceptors > 0) then
				
					! Write data in ST01 form
					if(this % iSrcType == 0) write(iLUN) this % rvFreeConc(:,iSpecies) * rFactor * rFactorConversion
				
				end if
				
				! Get concentrations from complex-terrain receptors
				if(this % iNumCTSGReceptors > 0) then
				
					! Write data in ST01 form
					if(this % iSrcType == 0) write(iLUN) this % rvCTSGConc(:,iSpecies) * rFactor * rFactorConversion
				
				end if
				
			end if
        
        end do

	end function writeCalpuff01
	
	
	function writeCalpuffComment(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Connect output file and write all comment information into it
		open(iLUN, file = tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		do i = 1, size(this % svCommentLine)
			write(iLUN, "(a)") trim(this % svCommentLine(i))
		end do
		close(iLUN)
		
	end function writeCalpuffComment


	function writeCalpuffHeader(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: i, iSpecies
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Connect output file and write all comment information into it
		open(iLUN, file = tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		write(iLUN, "('Identification')")
		write(iLUN, "('==============')")
		write(iLUN, "(' ')")
		write(iLUN,'("DataSet: ", a, "  DataVersion: ", a, "  DataModel: ", a)') &
			TRIM(this % sDataSet), TRIM(this % sDataVersion), TRIM(this % sDataModel)
		write(iLUN,"('File produced using model: ',a,' ',a,'    Level:',a)") &
			TRIM(this % sModel), TRIM(this % sVersion), TRIM(this % sLevel)
		write(iLUN, "(' ')")
		write(iLUN, "('This run title')")
		write(iLUN, "('==============')")
		write(iLUN, "(' ')")
		write(iLUN, "('T1> ', a)") TRIM(this % svTitle(1))
		write(iLUN, "('T2> ', a)") TRIM(this % svTitle(2))
		write(iLUN, "('T3> ', a)") TRIM(this % svTitle(3))
		write(iLUN, "(' ')")
		write(iLUN, "('Reference system')")
		write(iLUN, "('================')")
		write(iLUN, "(' ')")
		write(iLUN, "('Reference : ',a,'   Zone : ',i2)") TRIM(this % sPmap),this % iUTMZone
		write(iLUN, "('Datum     : ',a)") TRIM(this % sDatum)
		write(iLUN, "(' ')")
		write(iLUN, "('Timing')")
		write(iLUN, "('======')")
		write(iLUN, "(' ')")
		write(iLUN,"('Start year: ',i4,'  Julian day: ',i3,'  Hour: ',i2)") &
			this % iBeginYear, this % iBeginJulday, this % iBeginHour
		write(iLUN,"('Model steps: ',i6, '  Avg.time: ',i4)") &
			this % iModelSteps, this % iAveragingTime
		write(iLUN,"('Number of surface stations used: ', i5)") this % iNumSurfaceStations
		write(iLUN, "(' ')")
		write(iLUN, "('Grids')")
		write(iLUN, "('=====')")
		write(iLUN, "(' ')")
		write(iLUN,"('Time zone: ',a,')')") this % sTimeZone
		write(iLUN,"('SW point: (',f8.0,',',f8.0,')')") this % rX0*1000.,this % rY0*1000.
		write(iLUN,"('Nodes: X:',i7,' Y:',i7)") this % iNx,this % iNy
		write(iLUN,"('Delta: X:',f7.1,' Y:',f7.1)") this % rDx*1000.,this % rDy*1000.
		write(iLUN,"('Levels used in concentration modeling: ',i7)") this % iNumLevels
		write(iLUN,"('Comp.grid Imin: ',i5,'   Imax: ',i5,' Jmin: ',i5,'   Jmax: ',i5)") &
			this % iMinCompX, this % iMaxCompX, this % iMinCompY, this % iMaxCompY
		write(iLUN,"('Comp.grid Imin: ',i5,'   Imax: ',i5,' Jmin: ',i5,'   Jmax: ',i5)") &
			this % iMinSampX, this % iMaxSampX, this % iMinSampY, this % iMaxSampY
		write(iLUN,"('Mesh divisions: ',i5)") this % iMeshFactor
		write(iLUN, "(' ')")
		write(iLUN, "('Fixed emission point sources')")
		write(iLUN, "('============================')")
		write(iLUN, "(' ')")
		if(this % iNumPointSources1 > 0) then
			do i = 1, this % iNumPointSources1
				write(iLUN,"(a)") this % svPointSource1(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Modulated emission point sources')")
		write(iLUN, "('================================')")
		write(iLUN, "(' ')")
		if(this % iNumPointSources2 > 0) then
			do i = 1, this % iNumPointSources2
				write(iLUN,"(a)") this % svPointSource2(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Fixed emission area sources')")
		write(iLUN, "('===========================')")
		write(iLUN, "(' ')")
		if(this % iNumAreaSources1 > 0) then
			do i = 1, this % iNumAreaSources1
				write(iLUN,"(a)") this % svAreaSource1(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Modulated emission area sources')")
		write(iLUN, "('===============================')")
		write(iLUN, "(' ')")
		if(this % iNumAreaSources2 > 0) then
			do i = 1, this % iNumAreaSources2
				write(iLUN,"(a)") this % svAreaSource2(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Fixed emission line sources')")
		write(iLUN, "('===========================')")
		write(iLUN, "(' ')")
		if(this % iNumLineSources1 > 0) then
			do i = 1, this % iNumLineSources1
				write(iLUN,"(a)") this % svLineSource1(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Modulated emission line sources')")
		write(iLUN, "('===============================')")
		write(iLUN, "(' ')")
		if(this % iNumLineSources2 > 0) then
			do i = 1, this % iNumLineSources2
				write(iLUN,"(a)") this % svLineSource2(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Fixed emission volume sources')")
		write(iLUN, "('=============================')")
		write(iLUN, "(' ')")
		if(this % iNumVolumeSources1 > 0) then
			do i = 1, this % iNumVolumeSources1
				write(iLUN,"(a)") this % svVolumeSource1(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Modulated emission volume sources')")
		write(iLUN, "('=================================')")
		write(iLUN, "(' ')")
		if(this % iNumVolumeSources2 > 0) then
			do i = 1, this % iNumVolumeSources2
				write(iLUN,"(a)") this % svVolumeSource2(i)
			end do
		else
			write(iLUN, "('--none--')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Concentration totalization')")
		write(iLUN, "('==========================')")
		write(iLUN, "(' ')")
		if(this % iMsource == 0) then
			write(iLUN, "('Sources contribution totalized on output; no partial concentration reported')")
		else
			write(iLUN, "('Sources contribution totalized on output, and partial concentration reported too')")
		end if
		write(iLUN, "(' ')")
		write(iLUN, "('Receptor types present in file')")
		write(iLUN, "('==============================')")
		write(iLUN, "(' ')")
		if(this % lSamplingGrid) &
			write(iLUN, "('Gridded - W-E cells: ',i4,'    S-N cells:',i4)") this % iNxSampling, this % iNySampling
		if(this % iNumFreeReceptors>0) &
			write(iLUN, "('User-defined (free) : ',i3)") this % iNumFreeReceptors
		if(this % iNumCTSGReceptors>0) &
			write(iLUN, "('Complex terrain     : ',i3)") this % iNumCTSGReceptors
		write(iLUN, "(' ')")
		write(iLUN, "('Chemical species modeled')")
		write(iLUN, "('========================')")
		write(iLUN, "(' ')")
		do iSpecies = 1, this % iNumChemicalSpecies
			write(iLUN, "('  ',a,' (',a,')')") &
				TRIM(this % svSpeciesName(iSpecies)), &
				TRIM(this % svSpeciesUnit(iSpecies))
		end do
		close(iLUN)
		
	end function writeCalpuffHeader


	function openCalpuffSeries(this, iLUN, tOpts, iSeries, jSeries, iSeriesSup, jSeriesSup, rvWeight) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer, intent(out)					:: iSeries
		integer, intent(out)					:: jSeries
		integer, intent(out)					:: iSeriesSup
		integer, intent(out)					:: jSeriesSup
        real, dimension(4), intent(out)         :: rvWeight
		integer									:: iRetCode
		
		! Locals
		integer	:: iErrCode
		real(8)	:: rSx
		real(8)	:: rSy
		real	:: rDelta
		integer	:: i
        real    :: rMinX
        real    :: rMaxX
        real    :: rMinY
        real    :: rMaxY
        real    :: rMinDistance
        integer, dimension(1)   :: ivPosMinDistance
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write grid information in the form expected by ST post-processing chain
		open(iLUN, FILE=tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Find the receptor point closest to passed coordinates
		rDelta = 1000.0*(this % rDx / this % iMeshFactor)
		rSx = 1000.0*(this % rX0 + (rDelta/1000.0) * (this % iMinSampX - 1) + 0.5 * this % rDx)
		rSy = 1000.0*(this % rY0 + (rDelta/1000.0) * (this % iMinSampY - 1) + 0.5 * this % rDy)
		iSeries = floor((tOpts % rXp - rSx) / rDelta) + 1
		jSeries = floor((tOpts % rYp - rSy) / rDelta) + 1
		if(iSeries < 1 .or. iSeries > this % iNxSampling) then
			iRetCode = 2
			return
		end if
		if(jSeries < 1 .or. jSeries > this % iNySampling) then
			iRetCode = 3
			return
		end if
        if(iSeries < this % iNxSampling) then
            iSeriesSup = iSeries + 1
        else
            iSeriesSup = iSeries
        end if
        if(jSeries < this % iNySampling) then
            jSeriesSup = jSeries + 1
        else
            jSeriesSup = jSeries
        end if
        rMinX = rSx + rDelta*(iSeries - 1)
        rMinY = rSy + rDelta*(jSeries - 1)
        rMaxX = rSx + rDelta*(iSeriesSup - 1)
        rMaxY = rSy + rDelta*(jSeriesSup - 1)
        
        ! Compute inverse-squared-distance weights, starting from the squared distances between any of the four bounding points
        ! and the current point
        rvWeight(1) = (tOpts % rXp - rMinX)**2 + (tOpts % rYp - rMinY)**2
        rvWeight(2) = (tOpts % rXp - rMaxX)**2 + (tOpts % rYp - rMinY)**2
        rvWeight(3) = (tOpts % rXp - rMinX)**2 + (tOpts % rYp - rMaxY)**2
        rvWeight(4) = (tOpts % rXp - rMaxX)**2 + (tOpts % rYp - rMaxY)**2
        rMinDistance = minval(rvWeight)
        if(rMinDistance <= 1.e-1) then
            ivPosMinDistance = minloc(rvWeight)
            rvWeight                      = 0.0
            rvWeight(ivPosMinDistance(1)) = 1.0
        else
            rvWeight = 1. / rvWeight
            rvWeight = rvWeight / sum(rvWeight)
        end if
		
		! Write header
		write(iLUN, "('Time.Stamp')", advance="no")
		do i = 1, size(this % svSpeciesName)
			write(iLUN, "(',',a)", advance="no") trim(this % svSpeciesName(i))
		end do
		write(iLUN, "(1x)")
		
	end function openCalpuffSeries


	function writeCalpuffSeries(this, iLUN, tOpts, iSeries, jSeries, iSeriesSup, jSeriesSup, rvWeight, lARM2) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer, intent(in)						:: iSeries
		integer, intent(in)						:: jSeries
		integer, intent(out)					:: iSeriesSup
		integer, intent(out)					:: jSeriesSup
        real, dimension(4), intent(out)         :: rvWeight
        logical, intent(in), optional			:: lARM2
		integer									:: iRetCode
		
		! Locals
		integer	:: iSpecies
		real	:: rFactor
		real	:: rFactorConversion
		logical	:: lIsARM2
        real, dimension(4)  :: rvValue
        real				:: rInterpolated
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize parameters from options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		
		! Check ARM2 estimation for NO2 starting from NOx is to be used
		lIsARM2 = .false.
		if(present(lARM2)) then
			lIsARM2 = lARM2
		end if
		
		! Perform ARM2 correction, if requested
		if(lIsARM2) call correct_arm2(this % rmConc)
		
		! Get gridded concentrations, if any
		if(this % lSamplingGrid) then
		
			if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
                
				! Write data in time series form
				write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", advance="no") &
					this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
					this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay
				do iSpecies = 1, size(this % svSpeciesName)
                    rvValue(1) = this % rmConc(iSeries,jSeries,iSpecies) * rFactor * rFactorConversion
                    rvValue(2) = this % rmConc(iSeriesSup,jSeries,iSpecies) * rFactor * rFactorConversion
                    rvValue(3) = this % rmConc(iSeries,jSeriesSup,iSpecies) * rFactor * rFactorConversion
                    rvValue(4) = this % rmConc(iSeriesSup,jSeriesSup,iSpecies) * rFactor * rFactorConversion
                    rInterpolated = dot_product(rvValue, rvWeight)
					write(iLUN, "(',',e15.7)", advance="no") rInterpolated
				end do
                
				write(iLUN, "(1x)")
				
			end if
		
		end if
            
	end function writeCalpuffSeries
	
	
	function openCalpuffMultiSeries( &
		this, &
		iLUN, &
		tOpts, &
		ivSeries, jvSeries, ivSeriesSup, jvSeriesSup, rmWeight &
	) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)							:: this
		integer, intent(in)											:: iLUN
		type(ConcDecodeOptsType), intent(in)						:: tOpts
		integer, dimension(:), allocatable, intent(out)				:: ivSeries
		integer, dimension(:), allocatable, intent(out)				:: jvSeries
		integer, dimension(:), allocatable, intent(out)				:: ivSeriesSup
		integer, dimension(:), allocatable, intent(out)				:: jvSeriesSup
        real, dimension(:,:), allocatable, intent(out)         		:: rmWeight
		integer														:: iRetCode
		
		! Locals
		integer	:: iErrCode
		real(8)	:: rSx
		real(8)	:: rSy
		real	:: rDelta
		integer	:: i
        real    :: rMinX
        real    :: rMaxX
        real    :: rMinY
        real    :: rMaxY
        real    :: rMinDistance
        integer, dimension(1)							:: ivPosMinDistance
        real(8), dimension(:), allocatable				:: rvX0, rvY0
		character(len=256), dimension(:), allocatable	:: svPointName
        integer											:: iNumPoints
        integer											:: iPoint
        character(len=128)								:: sBuffer
        character(len=128)								:: sChem
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Clean workspace
		if(allocated(ivSeries))    deallocate(ivSeries)
		if(allocated(jvSeries))    deallocate(jvSeries)
		if(allocated(ivSeriesSup)) deallocate(ivSeriesSup)
		if(allocated(jvSeriesSup)) deallocate(jvSeriesSup)
		if(allocated(rmWeight))    deallocate(rmWeight)
		
		! Get points list, and reserve workspace
		open(iLUN, file=tOpts % sPointListFile, status='old', action='read', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		iNumPoints = 0
		do
			read(iLUN, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			iNumPoints = iNumPoints + 1
		end do
		if(iNumPoints <= 0) then
			iRetCode = 2
			return
		end if
		rewind(iLUN)
		allocate(rvX0(iNumPoints), rvY0(iNumPoints))
		allocate(svPointName(iNumPoints))
		allocate(ivSeries(iNumPoints))
		allocate(jvSeries(iNumPoints))
		allocate(ivSeriesSup(iNumPoints))
		allocate(jvSeriesSup(iNumPoints))
		allocate(rmWeight(4, iNumPoints))
		do i = 1, iNumPoints
			read(iLUN, *, iostat=iErrCode) svPointName(i), rvX0(i), rvY0(i)
			if(iErrCode /= 0) then
				close(iLUN)
				deallocate(rvX0, rvY0)
				deallocate(svPointName)
				deallocate(ivSeries)
				deallocate(jvSeries)
				deallocate(ivSeriesSup)
				deallocate(jvSeriesSup)
				deallocate(rmWeight)
				iRetCode = 3
				return
			end if
		end do
		close(iLUN)
		
		! Prepare to write data
		open(iLUN, FILE=tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		
		! Main loop: iterate over points
		do iPoint = 1, iNumPoints
		
			! Find the receptor point closest to passed coordinates
			rDelta = 1000.0*(this % rDx / this % iMeshFactor)
			rSx = 1000.0*(this % rX0 + this % rDx * (this % iMinSampX - 1) + 0.5 * this % rDx)
			rSy = 1000.0*(this % rY0 + this % rDy * (this % iMinSampY - 1) + 0.5 * this % rDy)
			ivSeries(iPoint) = floor((rvX0(iPoint) - rSx) / rDelta) + 1
			jvSeries(iPoint) = floor((rvY0(iPoint) - rSy) / rDelta) + 1
			if(ivSeries(iPoint) < 1 .or. ivSeries(iPoint) > this % iNxSampling) then
				iRetCode = 5
				return
			end if
			if(jvSeries(iPoint) < 1 .or. jvSeries(iPoint) > this % iNySampling) then
				iRetCode = 6
				return
			end if
			if(ivSeries(iPoint) < this % iNxSampling) then
				ivSeriesSup(iPoint) = ivSeries(iPoint) + 1
			else
				ivSeriesSup(iPoint) = ivSeries(iPoint)
			end if
			if(jvSeries(iPoint) < this % iNySampling) then
				jvSeriesSup(iPoint) = jvSeries(iPoint) + 1
			else
				jvSeriesSup(iPoint) = jvSeries(iPoint)
			end if
			rMinX = rSx + rDelta*(ivSeries(iPoint) - 1)
			rMinY = rSy + rDelta*(jvSeries(iPoint) - 1)
			rMaxX = rSx + rDelta*(ivSeriesSup(iPoint) - 1)
			rMaxY = rSy + rDelta*(jvSeriesSup(iPoint) - 1)
			
			! Compute inverse-squared-distance weights, starting from the squared distances between any of the four bounding points
			! and the current point
			rmWeight(1,iPoint) = (rvX0(iPoint) - rMinX)**2 + (rvY0(iPoint) - rMinY)**2
			rmWeight(2,iPoint) = (rvX0(iPoint) - rMaxX)**2 + (rvY0(iPoint) - rMinY)**2
			rmWeight(3,iPoint) = (rvX0(iPoint) - rMinX)**2 + (rvY0(iPoint) - rMaxY)**2
			rmWeight(4,iPoint) = (rvX0(iPoint) - rMaxX)**2 + (rvY0(iPoint) - rMaxY)**2
			rMinDistance = minval(rmWeight(:,iPoint))
			if(rMinDistance <= 1.e-1) then
				ivPosMinDistance = minloc(rmWeight(:,iPoint))
				rmWeight(:,iPoint)                   = 0.0
				rmWeight(ivPosMinDistance(1),iPoint) = 1.0
			else
				rmWeight(:,iPoint) = 1. / rmWeight(:,iPoint)
				rmWeight(:,iPoint) = rmWeight(:,iPoint) / sum(rmWeight(:,iPoint))
			end if
			
        end do
        
        ! Reclaim workspace
        deallocate(rvX0, rvY0)
		
		! Write header
		write(iLUN, "('Time.Stamp')", advance="no")
		sChem = this % svSpeciesName(tOpts % iSpeciesIdx)
		do i = 1, iNumPoints
			write(iLUN, "(',',a,'.',a)", advance="no") trim(sChem), trim(svPointName(i))
		end do
		write(iLUN, "(1x)")
		
	end function openCalpuffMultiSeries


	function writeCalpuffMultiSeries( &
		this, &
		iLUN, &
		tOpts, &
		ivSeries, jvSeries, ivSeriesSup, jvSeriesSup, rmWeight, &
		iSpeciesIdx, &
		lARM2 &
	) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)				:: this
		integer, intent(in)								:: iLUN
		type(ConcDecodeOptsType), intent(in)			:: tOpts
		integer, dimension(:), allocatable, intent(in)	:: ivSeries
		integer, dimension(:), allocatable, intent(in)	:: jvSeries
		integer, dimension(:), allocatable, intent(in)	:: ivSeriesSup
		integer, dimension(:), allocatable, intent(in)	:: jvSeriesSup
        real, dimension(:,:), allocatable, intent(in)	:: rmWeight
        integer, intent(in)								:: iSpeciesIdx
        logical, intent(in), optional					:: lARM2
		integer											:: iRetCode
		
		! Locals
		integer				:: i
		real				:: rFactor
		real				:: rFactorConversion
		logical				:: lIsARM2
        real, dimension(4)	:: rvValue
        real				:: rInterpolated
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize parameters from options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		
		! Check ARM2 estimation for NO2 starting from NOx is to be used
		lIsARM2 = .false.
		if(present(lARM2)) then
			lIsARM2 = lARM2
		end if
		
		! Perform ARM2 correction, if requested
		if(lIsARM2) call correct_arm2(this % rmConc)
		
		! Get gridded concentrations, if any
		if(this % lSamplingGrid) then
		
			if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
                
				! Write data in time series form
				write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", advance="no") &
					this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
					this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay
				do i = 1, size(ivSeries)
                    rvValue(1) = this % rmConc(ivSeries(i),jvSeries(i),iSpeciesIdx) * rFactor * rFactorConversion
                    rvValue(2) = this % rmConc(ivSeriesSup(i),jvSeries(i),iSpeciesIdx) * rFactor * rFactorConversion
                    rvValue(3) = this % rmConc(ivSeries(i),jvSeriesSup(i),iSpeciesIdx) * rFactor * rFactorConversion
                    rvValue(4) = this % rmConc(ivSeriesSup(i),jvSeriesSup(i),iSpeciesIdx) * rFactor * rFactorConversion
                    rInterpolated = dot_product(rvValue, rmWeight(:,i))
					write(iLUN, "(',',e15.7)", advance="no") rInterpolated
				end do
                
				write(iLUN, "(1x)")
				
			end if
		
		end if
            
	end function writeCalpuffMultiSeries
	
	
	function openCalpuffMultiSeriesClosest( &
		this, &
		iLUN, &
		tOpts, &
		ivSeries, jvSeries &
	) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)							:: this
		integer, intent(in)											:: iLUN
		type(ConcDecodeOptsType), intent(in)						:: tOpts
		integer, dimension(:), allocatable, intent(out)				:: ivSeries
		integer, dimension(:), allocatable, intent(out)				:: jvSeries
		integer														:: iRetCode
		
		! Locals
		integer	:: iErrCode
		real(8)	:: rSx
		real(8)	:: rSy
		real	:: rDelta
		integer	:: i
		integer	:: iX
		integer	:: iY
        real    :: rDistance
        integer, dimension(1)							:: ivPosMinDistance
        real(8), dimension(:), allocatable				:: rvX0, rvY0
		character(len=256), dimension(:), allocatable	:: svPointName
        integer											:: iNumPoints
        integer											:: iPoint
        character(len=128)								:: sBuffer
        character(len=128)								:: sChem
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Clean workspace
		if(allocated(ivSeries)) deallocate(ivSeries)
		if(allocated(jvSeries)) deallocate(jvSeries)
		
		! Get points list, and reserve workspace
		open(iLUN, file=tOpts % sPointListFile, status='old', action='read', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		iNumPoints = 0
		do
			read(iLUN, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			iNumPoints = iNumPoints + 1
		end do
		if(iNumPoints <= 0) then
			iRetCode = 2
			return
		end if
		rewind(iLUN)
		allocate(rvX0(iNumPoints), rvY0(iNumPoints))
		allocate(svPointName(iNumPoints))
		allocate(ivSeries(iNumPoints))
		allocate(jvSeries(iNumPoints))
		do i = 1, iNumPoints
			read(iLUN, *, iostat=iErrCode) svPointName(i), rvX0(i), rvY0(i)
			if(iErrCode /= 0) then
				close(iLUN)
				deallocate(rvX0, rvY0)
				deallocate(svPointName)
				deallocate(ivSeries)
				deallocate(jvSeries)
				iRetCode = 3
				return
			end if
		end do
		close(iLUN)
		
		! Find the indices of the closest receptors to assigned points
		rDelta = 1000.0*(this % rDx / this % iMeshFactor)
		rSx = 1000.0*(this % rX0 + this % rDx * (this % iMinSampX - 1) + 0.5 * this % rDx)
		rSy = 1000.0*(this % rY0 + this % rDy * (this % iMinSampY - 1) + 0.5 * this % rDy)
		do iPoint = 1, iNumPoints
			ivSeries(iPoint) = nint((rvX0(iPoint) - rSx)/rDelta) + 1
			jvSeries(iPoint) = nint((rvY0(iPoint) - rSy)/rDelta) + 1
		end do
		
		! Generate point list report
		open(iLUN, FILE = tOpts % sPointListReport, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		write(iLUN, "('Point.Name, X, Y, iX, iY')")
		do iPoint = 1, iNumPoints
			write(iLUN, "(a16,2(',',f9.1),2(',',i4))") &
				svPointName(iPoint), &
				rvX0(iPoint), rvY0(iPoint), &
				ivSeries(iPoint), jvSeries(iPoint)
		end do
		close(iLUN)
		
		! Prepare to write data
		open(iLUN, FILE=tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		
		! Write header
		write(iLUN, "('Time.Stamp')", advance="no")
		sChem = this % svSpeciesName(tOpts % iSpeciesIdx)
		do i = 1, iNumPoints
			write(iLUN, "(',',a,'.',a)", advance="no") trim(sChem), trim(svPointName(i))
		end do
		write(iLUN, "(1x)")
		
	end function openCalpuffMultiSeriesClosest


	function writeCalpuffMultiSeriesClosest( &
		this, &
		iLUN, &
		tOpts, &
		ivSeries, jvSeries, &
		iSpeciesIdx, &
		lARM2 &
	) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)				:: this
		integer, intent(in)								:: iLUN
		type(ConcDecodeOptsType), intent(in)			:: tOpts
		integer, dimension(:), allocatable, intent(in)	:: ivSeries
		integer, dimension(:), allocatable, intent(in)	:: jvSeries
        integer, intent(in)								:: iSpeciesIdx
        logical, intent(in), optional					:: lARM2
		integer											:: iRetCode
		
		! Locals
		integer				:: i
		real				:: rFactor
		real				:: rFactorConversion
		logical				:: lIsARM2
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize parameters from options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		
		! Check ARM2 estimation for NO2 starting from NOx is to be used
		lIsARM2 = .false.
		if(present(lARM2)) then
			lIsARM2 = lARM2
		end if
		
		! Perform ARM2 correction, if requested
		if(lIsARM2) call correct_arm2(this % rmConc)
		
		! Get gridded concentrations, if any
		if(this % lSamplingGrid) then
		
			if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
                
				! Write data in time series form
				write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", advance="no") &
					this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
					this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay
				do i = 1, size(ivSeries)
					write(iLUN, "(',',e15.7)", advance="no") this % rmConc(ivSeries(i),jvSeries(i),iSpeciesIdx) * rFactor * rFactorConversion
				end do
                
				write(iLUN, "(1x)")
				
			end if
		
		end if
            
	end function writeCalpuffMultiSeriesClosest
	
	
	function openCalpuffSummary(this, iLUN, sPrefix) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sPrefix
		integer								:: iRetCode
		
		! Locals
		integer				:: iSpecies
		character(len=256)	:: sFile
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write grid information in the form expected by ST post-processing chain
		do iSpecies = 1, size(this % svSpeciesName)
			write(sFile, "(a,'_',a,'.series.csv')") trim(sPrefix), trim(this % svSpeciesName(iSpecies))
			open(iLUN+iSpecies, file=sFile, status='unknown', action='write')
			write(iLUN+iSpecies, "('Time.Stamp, Mean.Conc, Max.Conc, Peak.Ratio, Mean.Nonzero.Conc, I.Max, J.Max, Num.Nonzero.Conc')")
		end do

	end function openCalpuffSummary
	
	
	function writeCalpuffSummary(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: iSpecies
		real	:: rConcRatio
		real	:: rMeanNonzeroConc
		integer	:: iNumNonzeroConc
		integer	:: iSeries
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
        ! Get concentration for each species
        do iSpecies = 1, size(this % svSpeciesName)
        
            ! Get gridded concentrations, if any
            if(this % lSamplingGrid) then
            
				! Add row to diagnostic series (computed only on gridded data)
				if(this % iSrcType == 0) then
					if(MAXVAL(this % rmConc(:,:,iSpecies)) > 0.0) then
						rConcRatio = MAXVAL(this % rmConc(:,:,iSpecies)) / &
							(SUM(this % rmConc(:,:,iSpecies))/SIZE(this % rmConc(:,:,iSpecies)))
						iNumNonzeroConc  = COUNT(this % rmConc(:,:,iSpecies) > 0.0)
						rMeanNonzeroConc = SUM(this % rmConc(:,:,iSpecies)) / iNumNonzeroConc
						write( &
							100+iSpecies, &
							"(i4.4,2('-',i2.2),' ',i2.2,2(':',i2.2),4(',',e15.7),2(',',i4),',',i8)" &
						) &
							this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
							this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay, &
							SUM(this % rmConc(:,:,iSpecies))/SIZE(this % rmConc(:,:,iSpecies)), &
							MAXVAL(this % rmConc(:,:,iSpecies)), &
							rConcRatio, &
							rMeanNonzeroConc, &
							MAXLOC(this % rmConc(:,:,iSpecies)), &
							iNumNonzeroConc
					else
						write( &
							100+iSpecies, &
							"(i4.4,2('-',i2.2),' ',i2.2,2(':',i2.2),4(',',e15.7),2(',',i4),',',i8)" &
						) &
							this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
							this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay, &
							SUM(this % rmConc(:,:,iSpecies))/SIZE(this % rmConc(:,:,iSpecies)), &
							MAXVAL(this % rmConc(:,:,iSpecies)), &
							0.0, 0.0, 0, 0, 0
					end if
				end if
					
            end if
            
        end do
        
	end function writeCalpuffSummary
	
	
	function closeCalpuffSummary(this, iLUN, sPrefix) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sPrefix
		integer								:: iRetCode
		
		! Locals
		integer				:: iSpecies
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write grid information in the form expected by ST post-processing chain
		do iSpecies = 1, size(this % svSpeciesName)
			close(iLUN+iSpecies)
		end do

	end function closeCalpuffSummary
	
	
	function openCalpuffMaxima(this, iLUN, sFile) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		integer, intent(in)					:: iLUN
		character(len=*), intent(in)		:: sFile
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write grid information in the form expected by ST post-processing chain
		open(iLUN, FILE=sFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Write header
		write(iLUN, "('Time.Stamp')", advance="no")
		do i = 1, size(this % svSpeciesName)
			write(iLUN, "(',',a,'.Max',',',a,'.I',',',a,'.J')", advance="no") &
				trim(this % svSpeciesName(i)), &
				trim(this % svSpeciesName(i)), &
				trim(this % svSpeciesName(i))
		end do
		write(iLUN, "(1x)")

	end function openCalpuffMaxima


	function writeCalpuffMaxima(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iSpecies
		integer	:: i
		real	:: rFactor
		real	:: rFactorConversion
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize parameters from options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		
        ! Get concentration for each species
        do iSpecies = 1, size(this % svSpeciesName)
        
            ! Get gridded concentrations, if any
            if(this % lSamplingGrid) then
            
                if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
                
	                ! Write data in time series form
	                write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", advance="no") &
						this % iYearDisplay, this % iMonthDisplay, this % iDayDisplay, &
						this % iHourDisplay, this % iMinuteDisplay, this % iSecondDisplay
					do i = 1, size(this % svSpeciesName)
						write(iLUN, "(',',e15.7,2(',',i4))", advance="no") &
							maxval(this % rmConc(:,:,iSpecies) * rFactor * rFactorConversion), &
							maxloc(this % rmConc(:,:,iSpecies))
					end do
					write(iLUN, "(1x)")
	                
	            end if
            
            end if
            
        end do

	end function writeCalpuffMaxima
	
	
	function updateCalpuffExceedances(this, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: i
		real	:: rFactorConversion
		real	:: rFactor
		real	:: rThreshold
		integer	:: iSpecies
		integer	:: iDesiredSpecies
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize parameters from options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		rThreshold        = tOpts % rThreshold
		iDesiredSpecies   = tOpts % iSpeciesIdx
		
        ! Get concentration for each species
        do iSpecies = 1, this % iNumChemicalSpecies
        
			if(iSpecies == iDesiredSpecies) then
        
				! Get gridded concentrations, if any
				if(this % lSamplingGrid) then
				
					if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
					
						! Update exceedances counter
						where(this % rmConc(:,:,iSpecies) * rFactorConversion * rFactor > rThreshold)
							this % imExceedances(:,:,iSpecies) = this % imExceedances(:,:,iSpecies) + 1
						endwhere
						
					end if
					
				end if
				
			end if
			
		end do

	end function updateCalpuffExceedances


	function resetCalpuffExceedances(this) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		! --none--
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Update exceedances counter
		this % imExceedances = 0

	end function resetCalpuffExceedances


	function writeCalpuffExceedances(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: iDesiredSpecies
		integer	:: iSpecies
		integer	:: iX, iY
		real(8)	:: rDelta
		real	:: rFactorConversion
		real	:: rFactor
		real	:: rX, rY
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Connect output file
		open(iLUN, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCOde = 1
			return
		end if
		write(iLUN, "('X, Y, Num.Exceedances')")
		
		! Initialize parameters from options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		iDesiredSpecies   = tOpts % iSpeciesIdx
		rDelta            = this % rDx / this % iMeshFactor
		
        ! Get concentration for each species
        do iSpecies = 1, this % iNumChemicalSpecies
        
			if(iSpecies == iDesiredSpecies) then
        
				! Get gridded concentrations, if any
				if(this % lSamplingGrid) then
				
					if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
					
						! Write data
						do iY = 1, this % iNxSampling
							rY = this % rY0 + (this % iMinSampY - 1) * this % rDy + (iY-1)*rDelta + this % rDy / 2.0
							do iX = 1, this % iNySampling
								rX = this % rX0 + (this % iMinSampX - 1) * this % rDx + (iX-1)*rDelta + this % rDx / 2.0
								write(iLUN, "(f9.1,',',f9.1,',',i6)") 1000.d0*rX, 1000.d0*rY, this % imExceedances(iX,iY,iSpecies)
							end do
						end do
						
					end if
				
				end if
				
			end if
        
        end do
        
        close(iLUN)

	end function writeCalpuffExceedances
	
	
	function startCalpuffQuantile(this, rQuantile) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)	:: this
		real, intent(in)					:: rQuantile
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: iNumHours
		integer	:: iTileSize
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Compute the number of data corresponding to the quantile passed
		iNumHours = this % iModelSteps
		iTileSize = floor(iNumHours * (1. - rQuantile))
		if(iTileSize <= 0 .or. iTileSize >= iNumHours) then
			iRetCode = 1
			return
		end if
		if(allocated(this % rtMaxConc)) deallocate(this % rtMaxConc)
		allocate(this % rtMaxConc(iTileSize, this % iNxSampling, this % iNySampling))
		if(allocated(this % rmMinConc)) deallocate(this % rmMinConc)
		allocate(this % rmMinConc(this % iNxSampling, this % iNySampling))
		if(allocated(this % imMinConcIdx)) deallocate(this % imMinConcIdx)
		allocate(this % imMinConcIdx(this % iNxSampling, this % iNySampling, 1))
		this % rtMaxConc = 0.
		this % rmMinConc = 0.
		this % imMinConcIdx = 1
		
	end function startCalpuffQuantile
	
	
	function updateCalpuffQuantile(this, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer	:: iDesiredSpecies
		integer	:: iSpecies
		integer	:: iX, iY
		real(8)	:: rDelta
		real	:: rFactorConversion
		real	:: rFactor
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Initialize parameters from options
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		iDesiredSpecies   = tOpts % iSpeciesIdx
		rDelta            = this % rDx / this % iMeshFactor
		
        ! Get concentration for each species
        do iSpecies = 1, this % iNumChemicalSpecies
        
			if(iSpecies == iDesiredSpecies) then
        
				! Get gridded concentrations, if any
				if(this % lSamplingGrid) then
				
					if(this % iSrcType == 0) then	! iSrcType == 0 for "TOTAL" special source (which may be missing in case CALPUFF input is mis-configured
					
						! Update minimal values, if exceeded
						do iX = 1, this % iNxSampling
							do iY = 1, this % iNySampling
								if(this % rmConc(iX, iY, iSpecies) > this % rmMinConc(iX, iY)) then
									this % rtMaxConc(this % imMinConcIdx(iX, iY, 1), iX, iY) = &
										this % rmConc(iX, iY, iSpecies) * rFactorConversion * rFactor
									this % rmMinConc(iX, iY) = minval(this % rtMaxConc(:, iX, iY))
									this % imMinConcIdx(iX, iY, :) = minloc(this % rtMaxConc(:, iX, iY))
								end if
							end do
						end do
						
					end if
					
				end if
				
			end if
			
		end do
		
	end function updateCalpuffQuantile
	
	
	function writeCalpuffQuantile(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer		:: iErrCode
		integer		:: iX, iY
		real		:: rX, rY
		real(8)		:: rDelta
		real		:: rFactorConversion
		real		:: rFactor
		
		! Initialize
		rFactorConversion = tOpts % rFactorConversion
		rFactor           = tOpts % rScale
		
		! Write current species quantile values
		open(iLUN, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		write(iLUN, "('X, Y, Conc')")
		rDelta = this % rDx / this % iMeshFactor
		do iY = 1, this % iNySampling
			rY = this % rY0 + (this % iMinSampY - 1) * this % rDy + (iY-1)*rDelta + this % rDy / 2.0
			do iX = 1, this % iNySampling
				rX = this % rX0 + (this % iMinSampX - 1) * this % rDx + (iX-1)*rDelta + this % rDx / 2.0
				write(iLUN, "(f9.1,',',f9.1,',',e15.6)") &
					1000.d0*rX, 1000.d0*rY, &
					this % rmMinConc(iX,iY) * rFactorConversion * rFactor
			end do
		end do
		close(iLUN)
		
	end function writeCalpuffQuantile
	
	
	function writeCalpuffMean(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer		:: iErrCode
		integer		:: iX, iY
		real		:: rX, rY
		real(8)		:: rDelta
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write current species quantile values
		open(iLUN, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		write(iLUN, "('X, Y, Conc')")
		rDelta = this % rDx / this % iMeshFactor
		do iY = 1, this % iNySampling
			rY = this % rY0 + (this % iMinSampY - 1) * this % rDy + (iY-1)*rDelta + this % rDy / 2.0
			do iX = 1, this % iNySampling
				rX = this % rX0 + (this % iMinSampX - 1) * this % rDx + (iX-1)*rDelta + this % rDx / 2.0
				write(iLUN, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					this % rmSumConc(iX,iY,tOpts % iSpeciesIdx) / this % iNumConc &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(iLUN)
		
	end function writeCalpuffMean
	
	
	function writeCalpuffMax(this, iLUN, tOpts) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer									:: iRetCode
		
		! Locals
		integer		:: iErrCode
		integer		:: iX, iY
		real		:: rX, rY
		real(8)		:: rDelta
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Write current species quantile values
		open(iLUN, file=tOpts % sOutputFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		write(iLUN, "('X, Y, Conc')")
		rDelta = this % rDx / this % iMeshFactor
		do iY = 1, this % iNySampling
			rY = this % rY0 + (this % iMinSampY - 1) * this % rDy + (iY-1)*rDelta + this % rDy / 2.0
			do iX = 1, this % iNySampling
				rX = this % rX0 + (this % iMinSampX - 1) * this % rDx + (iX-1)*rDelta + this % rDx / 2.0
				write(iLUN, "(f9.1,',',f9.1,',',e15.6)") 1000.d0*rX, 1000.d0*rY, &
					this % rmMaxConc(iX,iY,tOpts % iSpeciesIdx) &
						* tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(iLUN)
		
	end function writeCalpuffMax
	
	
	function writeCalpuffConc(this, iLUN, tOpts, iNumThisFrame) result(iRetCode)
	
		! Routine arguments
		class(CalpuffType), intent(inout)		:: this
		integer, intent(in)						:: iLUN
		type(ConcDecodeOptsType), intent(in)	:: tOpts
		integer, intent(in)						:: iNumThisFrame
		integer									:: iRetCode
		
		! Locals
		integer		:: iErrCode
		integer		:: iX, iY
		real		:: rX, rY
		real(8)		:: rDelta
		character(len=256)	:: sFileName
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Form file name
		write(sFileName, "(a,'\\movie_',i6.6,'.csv')") trim(tOpts % sMoviePath), iNumThisFrame
		
		! Write current species "instant" values
		open(iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		write(iLUN, "('X, Y, Conc')")
		rDelta = this % rDx / this % iMeshFactor
		do iY = 1, this % iNySampling
			rY = this % rY0 + (this % iMinSampY - 1) * this % rDy + (iY-1)*rDelta + this % rDy / 2.0
			do iX = 1, this % iNySampling
				rX = this % rX0 + (this % iMinSampX - 1) * this % rDx + (iX-1)*rDelta + this % rDx / 2.0
				write(iLUN, "(f9.1,',',f9.1,',',e15.6)") &
					1000.d0*rX, 1000.d0*rY, &
					this % rmConc(iX,iY,tOpts % iSpeciesIdx) * tOpts % rFactorConversion * tOpts % rScale
			end do
		end do
		close(iLUN)
		
	end function writeCalpuffConc
	
	! *********************
	! * Internal routines *
	! *********************
	
	function DIR_WIND(rU,rV) result(rDir)
	
		! Routine arguments
		real, intent(in)	:: rU
		real, intent(in)	:: rV
		real				:: rDir
		
		if(abs(rU)<=0.0001 .and. abs(rV)<=0.0001) then
			rDir = 0.
			return
		endif
		rDir = ATAN2(rU,rV)*57.29578
		if(rDir < 0.) rDir = rDir + 360.
		
	end function DIR_WIND

    
    subroutine ExpandDate(iYearIn, iJuldayIn, iHourIn, iSecondIn, iYear, iMonth, iDay, iHour, iMinute, iSecond)
    
		! Routine arguments
		integer, intent(in)		:: iYearIn
		integer, intent(in)		:: iJulDayIn
		integer, intent(in)		:: iHourIn
		integer, intent(in)		:: iSecondIn
		integer, intent(out)	:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
		! Locals
		integer	:: iCurTime
		
		! Compute base time, then add shifts
		call PackTime(iCurTime, iYearIn, 1, 1, 0, 0, 0)
		iCurTime = iCurTime + 86400*(iJulDayIn-1) + 3600*iHourIn + iSecondIn
		
		! Expand to fully formatted time
		call UnpackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
		
    end subroutine ExpandDate
    
    
    elemental subroutine correct_arm2(c)
    
    	! Routine arguments
    	real, intent(inout)	:: c	! On entry, NOx in ug/m3; on exit, NO2 in ug/m3
    	
    	! Locals
    	real				:: ratio
    	
    	! Input is in g/m3: convert first to ug/m3 as prescribed by ARM2
    	c = c * 1.e6
    	
    	! Perform conversion
		ratio = ( &
				( &
					( &
						( &
							( &
								(-1.1723d-17*c + 4.2795d-14 &
							)*c - 5.8345d-11 &
						)*c + 3.4555d-08 &
					)*c - 5.6062d-06 &
				)*c - 2.7383d-03 &
			)*c + 1.2441d0 &
		)
		c = min(0.9, max(0.2, ratio)) * c
		
		! Convert back to g/m3
		c = c * 1.e-6
    
    end subroutine correct_arm2
    
end module Calpuff6Files
