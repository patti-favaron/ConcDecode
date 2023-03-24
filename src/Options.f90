! Options - Module supporting the parsing and treatment of standard options
!           for Calpuff support programs
!
! Copyright 2017 by Patrizia Favaron
module Options

	implicit none

	private

	! Public interface
	public	:: MetDecodeOptsType
	public	:: ConcDecodeOptsType

	type MetDecodeOptsType
		integer				:: iProcessingType
		character(len=64)	:: sOption
		character(len=256)	:: sCalmetFile
		character(len=256)	:: sOutputFile
		character(len=256)	:: sOutputPrefix
		character(len=256)	:: sSurfaceFile
		character(len=256)	:: sProfileFile
		character(len=256)	:: sGrADSCtl
		character(len=256)	:: sGrADSFile
		real				:: rXp
		real				:: rYp
		integer				:: iZ
	contains
		procedure	:: forMetDecode => forMetDecode
	end type MetDecodeOptsType

	type ConcDecodeOptsType
		integer				:: iProcessingType
		character(len=64)	:: sOption
		character(len=256)	:: sCalpuffFile
		character(len=256)	:: sOtherCalpuffFile
		character(len=256)	:: sOutputFile
		character(len=256)	:: sSurfaceFile
		character(len=256)	:: sProfileFile
		character(len=256)	:: sPointListFile
		character(len=256)	:: sPointListReport
		character(len=256)	:: sMoviePath
		real				:: rXp
		real				:: rYp
		integer				:: iZ
		real				:: rFactorConversion
		real				:: rScale
		real				:: rThreshold
		integer				:: iSpeciesIdx
		integer				:: iSampleSize
		real				:: rQuantile
		character(len=2048)	:: sProfileSpec
	contains
		procedure	:: forConcDecode => forConcDecode
	end type ConcDecodeOptsType

contains

	function forMetDecode(this) result(iRetCode)

		! Routine arguments
		class(MetDecodeOptsType), intent(out)	:: this
		integer									:: iRetCode

		! Locals
		integer				:: iErrCode
		logical				:: lIsFile
		character(len=64)	:: sBuffer
		integer				:: iPos

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check minimum parameter requirements
		if(command_argument_count() < 2) then
			print *,'MetDecode - Diagnostic program for Calmet output files'
			print *
			print *,'Usage:'
			print *
			print *,'  MetDecode <CalmetFile> <Option> [Parameters...]'
			print *
			print *,'Allowed <Options>:'
			print *
			print *,'  --check-topo <OutputFile>'
			print *
			print *,'            Perform basic topography check on DTM and grid data.'
			print *
			print *,'  --check-whole <OutputFile>'
			print *
			print *,'            Perform basic checks on wind field and temperature data.'
			print *
			print *,'  --check-point <Xp> <Yp> <OutputFile>'
			print *
			print *,'            Perform basic checks on wind field and temperature data.'
			print *
			print *,'  --check-site <Xp> <Yp> <OutputFile>'
			print *
			print *,'            Check site characteristics (e.g. height, slope, land use, ...)'
			print *,'            from CALMET file header.'
			print *
			print *,'  --check-point-hgtindex <Xp> <Yp> <Z_Index> <OutputFile>'
			print *
			print *,'            Perform basic checks on wind field and temperature data at a given height.'
			print *
			print *,'  --get-comment <OutputFile>'
			print *
			print *,'            Extract comment from CALMET file.'
			print *
			print *,'  --get-ctdm-met <Xp> <Yp> <SurfaceFile> <ProfileFile>'
			print *
			print *,'            Get 2D meteo data and save them to CTDM form, for direct Calpuff use'
			print *
			print *,'  --get-caline-met <Xp> <Yp> <SurfaceFile>'
			print *
			print *,'            Get 2D meteo data and save them to Caline form'
			print *
			print *,'  --get-profile <Xp> <Yp> <ProfileFile>'
			print *
			print *,'            Get 2D meteo data and save them to a standard textual form, for analysis'
			print *
			print *,'  --convert-grads <OutputGrADSPrefix>'
			print *
			print *,'            Convert file to GrADS binary form, and provide the CTL'
			print *,'            (<OutputGrADSPrefix> is the name of a file without extension)'
			print *
			print *,'  --get-snaps <LevelIdx> <OutputPrefix>'
			print *
			print *,'            Convert file to Snapshots at same given level'
			print *
			print *,'Copyright 2018 by Servizi Territorio srl'
			print *,'                  All rights reserved'
			stop
		end if
		call get_command_argument(1, this % sCalmetFile)
		call get_command_argument(2, this % sOption)
		inquire(file=this % sCalmetFile, exist=lIsFile)
		if(.not.lIsFile) then
			print *,"MetDecode:: error: Calmet file not found"
			stop
		end if

		! Perform option-specific processing
		this % iProcessingType = 0
		if(this % sOption == "--check-topo") then
			if(command_argument_count() < 3) then
				print *,"MetDecode:: error: Output file name not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 1
		elseif(this % sOption == "--check-whole") then
			if(command_argument_count() < 3) then
				print *,"MetDecode:: error: Output file name not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 2
		elseif(this % sOption == "--check-point") then
			if(command_argument_count() < 5) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, this % sOutputFile)
			this % iProcessingType = 3
		elseif(this % sOption == "--check-point-hgtindex") then
			if(command_argument_count() < 6) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iZ
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Z_Index> parameter is invalid"
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 4
		elseif(this % sOption == "--get-comment") then
			if(command_argument_count() < 3) then
				print *,"MetDecode:: error: Output file name not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 5
		elseif(this % sOption == "--check-site") then
			if(command_argument_count() < 5) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, this % sOutputFile)
			this % iProcessingType = 6
		elseif(this % sOption == "--get-ctdm-met") then
			if(command_argument_count() < 6) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, this % sSurfaceFile)
			call get_command_argument(6, this % sProfileFile)
			this % iProcessingType = 7
		elseif(this % sOption == "--get-profile") then
			if(command_argument_count() < 5) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, this % sProfileFile)
			this % iProcessingType = 8

		elseif(this % sOption == "--convert-grads") then

			if(command_argument_count() < 3) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, this % sGrADSFile)

			! Adjust file extensions so that data file has 'dat' and CTL file 'ctl'
			this % sGrADSCtl  = trim(this % sGrADSFile) // '.ctl'
			this % sGrADSFile = trim(this % sGrADSFile) // '.dat'
			this % iProcessingType = 9

		elseif(this % sOption == "--get-snaps") then

			if(command_argument_count() < 4) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iZ
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Level> parameter is invalid"
				stop
			end if
			call get_command_argument(4, this % sOutputPrefix)
			this % iProcessingType = 10

		elseif(this % sOption == "--get-caline-met") then

			if(command_argument_count() < 5) then
				print *,"MetDecode:: error: Less parameters than expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"MetDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, this % sSurfaceFile)
			this % iProcessingType = 11

		end if

	end function forMetDecode


	function forConcDecode(this) result(iRetCode)

		! Routine arguments
		class(ConcDecodeOptsType), intent(out)	:: this
		integer									:: iRetCode

		! Locals
		integer				:: iErrCode
		logical				:: lIsFile
		character(len=128)	:: sBuffer
		integer				:: iPos
		character			:: cFactor

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check minimum parameter requirements
		if(command_argument_count() < 2) then
			print *,'ConcDecode - Utility program for Calpuff concentration files'
			print *
			print *,'Usage:'
			print *
			print *,'  ConcDecode <CalpuffConcFile> <Option> [Parameters...]'
			print *
			print *,'Prepend a ''@'' character to <CalpuffConcDecode> for a Calpuff 7 file: otherwise,'
			print *,'Celpuff 6 run assumed.'
			print *
			print *,'Allowed <Options>:'
			print *
			print *,'  --get-grd <OutputFile>'
			print *
			print *,'            Get receptor grid file for use in ST standard chain'
			print *
			print *,'  --get-receptors <OutputFile>'
			print *
			print *,'            Get receptor list'
			print *
			print *,'  --get-mean <Unit> <Factor> <SpeciesIdx> <OutputPrefix>'
			print *
			print *,'            Get mean field (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-mean-arm2 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get mean field (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-max <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get max field (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-max-arm2 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get max field (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-max-mean-8 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get max of 8 hours means (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-max-mean-8-arm2 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get max of 8 hours means (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-mean-max-8 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get mean of 8 hours maxima (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-mean-max-8-arm2 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get mean of 8 hours maxima (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-max-mean-24 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get max of 24 hours means (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-max-mean-24-arm2 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get max of 24 hours means (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-mean-max-24 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get mean of 24 hours max (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-mean-max-24-arm2 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get mean of 24 hours max (for one species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-01 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Transform TOTAL concentration field to 01 files (one per species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-01-arm2 <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Transform TOTAL concentration field to 01 files (one per species)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-metadata <OutputFile>'
			print *
			print *,'            Get metadata stored in file, including grid definition.'
			print *
			print *,'  --get-comment <OutputFile>'
			print *
			print *,'            Extract comment from CALPUFF file.'
			print *
			print *,'  --get-timeseries <Xp> <Yp> <Unit> <Factor> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at a specified point'
			print *
			print *,'            <Xp>, <Yp> designate the position of a point, in metric units, and same'
			print *,'                       reference as adopted in simulation (e.g. UTM32 N)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-timeseries-arm2 <Xp> <Yp> <Unit> <Factor> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at a specified point'
			print *
			print *,'            <Xp>, <Yp> designate the position of a point, in metric units, and same'
			print *,'                       reference as adopted in simulation (e.g. UTM32 N)'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-multiseries-interp <PointListFile> <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at a specified point'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to interpolate'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-multiseries-interp-arm2 <PointListFile> <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at a specified point'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to interpolate'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-multiseries-closest <PointListFile> <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at points closest to'
			print *,'            modeled receptors'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to gather'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-multiseries-closest-arm2 <PointListFile> <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at points closest to'
			print *,'            modeled receptors'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to interpolate'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-multiseries-random-comparison <OtherCalpuffFile> <SampleSize> <Unit> <Factor> <SpeciesIdx> <OutPrefix>'
			print *
			print *,'            Get time series composed by all chemical species at points closest to'
			print *,'            modeled receptors'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to gather'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-multiseries-random-comp-arm2 <OtherCalpuffFile> <SampleSize> <Unit> <Factor> <SpeciesIdx> <OutPrefix>'
			print *
			print *,'            Get time series composed by all chemical species at points closest to'
			print *,'            modeled receptors'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to gather'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-summaries <OutputPrefix>'
			print *
			print *,'            Get a set of summary time serie files, as many as chemical species in file'
			print *
			print *,'  --get-maxima <Unit> <Factor> <OutputFile>'
			print *
			print *,'            Get maxima values and positions per species, in sequence.'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --get-exceedances <Unit> <Factor> <SpeciesIdx> <ConcThreshold> <OutputFile>'
			print *
			print *,'            Count concentration exceedances over a period'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'            <ConcThreshold> is expressed in the same units as concentration,'
			print *,'                            once multiplication by factor and unit conversion'
			print *,'                            have been made'
			print *
			print *,'    WARNING: --get-exceedances supported for gridded receptors only'
			print *
			print *,'  --get-exceedances-arm2 <Unit> <Factor> <SpeciesIdx> <ConcThreshold> <OutputFile>'
			print *
			print *,'            Count concentration exceedances over a period'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'            <ConcThreshold> is expressed in the same units as concentration,'
			print *,'                            once multiplication by factor and unit conversion'
			print *,'                            have been made'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'    WARNING: --get-exceedances supported for gridded receptors only'
			print *
			print *,'  --get-quantile <Unit> <Factor> <Quantile> <SpeciesIdx> <OutputPrefix>'
			print *
			print *,'            Get TOTAL concentration quantile for a species'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <Quantile> is the quantile value to compute (0 < q < 1)'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'  --get-quantile-arm2 <Unit> <Factor> <Quantile> <SpeciesIdx> <OutputPrefix>'
			print *
			print *,'            Get TOTAL concentration quantile for a species'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <Quantile> is the quantile value to compute (0 < q < 1)'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'  --get-quantile-mean-24 <Unit> <Factor> <Quantile> <SpeciesIdx> <OutputPrefix>'
			print *
			print *,'            Get TOTAL concentration quantile for daily means of a species'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <Quantile> is the quantile value to compute (0 < q < 1)'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'  --get-quantile-mean-8 <Unit> <Factor> <Quantile> <SpeciesIdx> <OutputPrefix>'
			print *
			print *,'            Get TOTAL concentration quantile for 8 hour means of a species'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <Quantile> is the quantile value to compute (0 < q < 1)'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'  --get-quantile-moving-average-8 <Unit> <Factor> <Quantile> <SpeciesIdx> <OutputPrefix>'
			print *
			print *,'            Get TOTAL concentration quantile for 8 hour moving avgs of a species'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <Quantile> is the quantile value to compute (0 < q < 1)'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'  --get-max-moving-average-8 <Unit> <Factor> <SpeciesIdx> <OutputPrefix>'
			print *
			print *,'            Get TOTAL concentration quantile for 8 hour moving avgs of a species'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'  --get-movie-data <Unit> <Factor> <SpeciesIdx> <OutputPath>'
			print *
			print *,'            Get TOTAL concentration movie as set of XYZ files, to be rendered graphically'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'   --field-compare <OtheCalpuffConcFile> <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Compare individual concentration fieldsfrom homologous Calpuff results'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            <SpeciesIdx> is the index of species desired, as in species list'
			print *
			print *,'  --compare-randomized <OtherCalpuffFile> <SampleSize> <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at points closest to'
			print *,'            modeled receptors'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to gather'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'  --compare-randomized-arm2 <OtherCalpuffFile> <SampleSize> <Unit> <Factor> <SpeciesIdx> <OutputFile>'
			print *
			print *,'            Get time series composed by all chemical species at points closest to'
			print *,'            modeled receptors'
			print *
			print *,'            <PointListFile> designate the name of a file containing the names and positions'
			print *,'                            of the series to gather'
			print *
			print *,'            <Unit> may be "u" for micro-grams per cubic meter, "m" for mg/m3, or "g" for g/m3'
			print *
			print *,'            <Factor> is a multiplication factor for improving mean computations in ST chain'
			print *
			print *,'            Concentrations are scaled using the ARM2 polynomial, to convert "NOx as NO2" to NO2'
			print *
			print *,'Copyright 2018 by Servizi Territorio srl'
			print *,'                  All rights reserved'
			stop
		end if
		call get_command_argument(1, this % sCalpuffFile)
		call get_command_argument(2, this % sOption)
		if(this % sCalpuffFile(1:1) == '@') then
			inquire(file=this % sCalpuffFile(2:), exist=lIsFile)
		else
			inquire(file=this % sCalpuffFile, exist=lIsFile)
		end if
		if(.not.lIsFile) then
			print *,"ConcDecode:: error: Calpuff concentration/deposition file not found"
			stop
		end if

		! Perform option-specific processing
		this % iProcessingType = 0
		if(this % sOption == "--get-metadata") then
			if(command_argument_count() < 3) then
				print *,"ConcDecode:: error: Output file name not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 1

		elseif(this % sOption == "--get-comment") then
			if(command_argument_count() < 3) then
				print *,"ConcDecode:: error: Output file name not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 2

		elseif(this % sOption == "--get-01") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 6 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 3

		elseif(this % sOption == "--get-grd") then
			if(command_argument_count() < 3) then
				print *,"ConcDecode:: error: Output file name not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 4

		elseif(this % sOption == "--get-timeseries") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 5

		elseif(this % sOption == "--get-receptors") then
			if(command_argument_count() < 3) then
				print *,"ConcDecode:: error: Output file name not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 6

		elseif(this % sOption == "--get-summaries") then
			if(command_argument_count() < 3) then
				print *,"ConcDecode:: error: Output file prefix not found"
				stop
			end if
			call get_command_argument(3, this % sOutputFile)
			this % iProcessingType = 7

		elseif(this % sOption == "--get-maxima") then
			if(command_argument_count() < 5) then
				print *,"ConcDecode:: error: Received less parameters than the 5 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, this % sOutputFile)
			this % iProcessingType = 8

		elseif(this % sOption == "--get-exceedances") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rThreshold
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid concentration threshold'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 9

		elseif(this % sOption == "--get-quantile") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rQuantile
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid quantile value'
				stop
			end if
			if(this % rQuantile <= 0. .or. this % rQuantile >= 1.) then
				print *,'ConcDecode:: error: Quantile value not in range 0 < q < 1'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 10

		elseif(this % sOption == "--get-movie-data") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sMoviePath)
			this % iProcessingType = 12

		elseif(this % sOption == "--get-mean") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 13

		elseif(this % sOption == "--get-max") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 14

		elseif(this % sOption == "--get-mean-max-8") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 15

		elseif(this % sOption == "--get-max-mean-8") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 16

		elseif(this % sOption == "--get-mean-max-24") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 17

		elseif(this % sOption == "--get-max-mean-24") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 18

		elseif(this % sOption == "--get-quantile-mean-24") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rQuantile
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid quantile value'
				stop
			end if
			if(this % rQuantile <= 0. .or. this % rQuantile >= 1.) then
				print *,'ConcDecode:: error: Quantile value not in range 0 < q < 1'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 19

		elseif(this % sOption == "--get-quantile-mean-8") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rQuantile
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid quantile value'
				stop
			end if
			if(this % rQuantile <= 0. .or. this % rQuantile >= 1.) then
				print *,'ConcDecode:: error: Quantile value not in range 0 < q < 1'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 20

		elseif(this % sOption == "--get-quantile-moving-average-8") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rQuantile
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid quantile value'
				stop
			end if
			if(this % rQuantile <= 0. .or. this % rQuantile >= 1.) then
				print *,'ConcDecode:: error: Quantile value not in range 0 < q < 1'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 23

		elseif(this % sOption == "--get-max-moving-average-8") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 6 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 21

		elseif(this % sOption == "--field-compare") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, this % sOtherCalpuffFile)
			call get_command_argument(4, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 25

		elseif(this % sOption == "--get-mean-arm2") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 26

		elseif(this % sOption == "--get-max-arm2") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 27

		elseif(this % sOption == "--get-max-mean-8-arm2") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 28

		elseif(this % sOption == "--get-mean-max-8-arm2") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 29

		elseif(this % sOption == "--get-max-mean-24-arm2") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 30

		elseif(this % sOption == "--get-mean-max-24-arm2") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 31

		elseif(this % sOption == "--get-timeseries-arm2") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rXp
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <Xp> parameter is invalid"
				stop
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rYp
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <Yp> parameter is invalid"
				stop
			end if
			call get_command_argument(5, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 32

		elseif(this % sOption == "--get-01-arm2") then
			if(command_argument_count() < 6) then
				print *,"ConcDecode:: error: Less parameters than the 6 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, this % sOutputFile)
			this % iProcessingType = 33

		elseif(this % sOption == "--get-exceedances-arm2") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rThreshold
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid concentration threshold'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 34

		elseif(this % sOption == "--get-quantile-arm2") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(4, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rQuantile
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid quantile value'
				stop
			end if
			if(this % rQuantile <= 0. .or. this % rQuantile >= 1.) then
				print *,'ConcDecode:: error: Quantile value not in range 0 < q < 1'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 35

		elseif(this % sOption == "--get-multiseries-interp") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sPointListFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <PointListFile> is invalid"
				stop
			end if
			call get_command_argument(4, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 36

		elseif(this % sOption == "--get-multiseries-interp-arm2") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sPointListFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <PointListFile> is invalid"
				stop
			end if
			call get_command_argument(4, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 37

		elseif(this % sOption == "--get-multiseries-closest") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sPointListFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <PointListFile> is invalid"
				stop
			end if
			this % sPointListReport = trim(this % sPointListFile) // ".report" 
			call get_command_argument(4, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 38

		elseif(this % sOption == "--get-multiseries-closest-arm2") then
			if(command_argument_count() < 7) then
				print *,"ConcDecode:: error: Less parameters than the 7 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sPointListFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <PointListFile> is invalid"
				stop
			end if
			this % sPointListReport = trim(this % sPointListFile) // ".report" 
			call get_command_argument(4, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(5, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(7, this % sOutputFile)
			this % iProcessingType = 39

		elseif(this % sOption == "--compare-randomized") then
			if(command_argument_count() < 8) then
				print *,"ConcDecode:: error: Less parameters than the 8 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sOtherCalpuffFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <OtherCalpuffFile> is invalid"
				stop
			end if
			this % sPointListReport = trim(this % sPointListFile) // ".randomization"
			call get_command_argument(4, sBuffer)
			read(sBuffer, *, iostat=iErrCode) this % iSampleSize
			if(this % iSampleSize <= 0) then
				print *,"ConcDecode:: error: <SampleSize> is zero or negative"
				stop
			end if
			call get_command_argument(5, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(7, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(8, this % sOutputFile)
			this % iProcessingType = 40

		elseif(this % sOption == "--compare-randomized-arm2") then
			if(command_argument_count() < 8) then
				print *,"ConcDecode:: error: Less parameters than the 8 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sOtherCalpuffFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <OtherCalpuffFile> is invalid"
				stop
			end if
			this % sPointListReport = trim(this % sPointListFile) // ".randomization"
			call get_command_argument(4, sBuffer)
			read(sBuffer, *, iostat=iErrCode) this % iSampleSize
			if(this % iSampleSize <= 0) then
				print *,"ConcDecode:: error: <SampleSize> is zero or negative"
				stop
			end if
			call get_command_argument(5, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(7, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(8, this % sOutputFile)
			this % iProcessingType = 41

		elseif(this % sOption == "--get-multiseries-random-comparison") then
			if(command_argument_count() < 8) then
				print *,"ConcDecode:: error: Less parameters than the 8 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sOtherCalpuffFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <OtherCalpuffFile> is invalid"
				stop
			end if
			this % sPointListReport = trim(this % sPointListFile) // ".randomization"
			call get_command_argument(4, sBuffer)
			read(sBuffer, *, iostat=iErrCode) this % iSampleSize
			if(this % iSampleSize <= 0) then
				print *,"ConcDecode:: error: <SampleSize> is zero or negative"
				stop
			end if
			call get_command_argument(5, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(7, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(8, this % sOutputFile)
			this % iProcessingType = 42

		elseif(this % sOption == "--get-multiseries-random-comp-arm2") then
			if(command_argument_count() < 8) then
				print *,"ConcDecode:: error: Less parameters than the 8 expected"
				stop
			end if
			call get_command_argument(3, sBuffer)
			read(sBuffer,"(a)",iostat=iErrCode) this % sOtherCalpuffFile
			if(iErrCode /= 0) then
				print *,"ConcDecode:: error: <OtherCalpuffFile> is invalid"
				stop
			end if
			this % sPointListReport = trim(this % sPointListFile) // ".randomization"
			call get_command_argument(4, sBuffer)
			read(sBuffer, *, iostat=iErrCode) this % iSampleSize
			if(this % iSampleSize <= 0) then
				print *,"ConcDecode:: error: <SampleSize> is zero or negative"
				stop
			end if
			call get_command_argument(5, cFactor)
			if(cFactor=='g') then
				this % rFactorConversion = 1.
			elseif(cFactor=='m') then
				this % rFactorConversion = 1.e3
			else
				! Here we are if sUnit=='u' and any other value
				this % rFactorConversion = 1.e6
			end if
			call get_command_argument(6, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % rScale
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid scaling factor value'
				stop
			end if
			call get_command_argument(7, sBuffer)
			read(sBuffer,*,iostat=iErrCode) this % iSpeciesIdx
			if(iErrCode /= 0) then
				print *,'ConcDecode:: error: Invalid species index'
				stop
			end if
			call get_command_argument(8, this % sOutputFile)
			this % iProcessingType = 43

		end if

	end function forConcDecode


	subroutine ToUpper(sString)

		! Routine arguments
		character(len=*), intent(inout)	:: sString

		! Locals
		integer		:: i
		character	:: ch

		! Convert characters to uppercase
		do i = 1, len_trim(sString)
			ch = sString(i:i)
			if('a' <= ch .and. ch <= 'z') then
				sString(i:i) = char(ichar(ch) - ichar('a') + ichar('A'))
			end if
		end do

	end subroutine ToUpper

end module Options
