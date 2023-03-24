module ModulationProfiles

	use Calendar
	
	implicit none
	
	private
	
	! Public interface
	public	:: ProfileType
	! Temporary
	public	:: CountCharacterOccurrences
	public	:: SplitToken
	
	! Data types
	
	type ProfileType
		real, dimension(7)		:: rvWeekday
		real, dimension(12)		:: rvMonth
		real, dimension(0:23)	:: rvHour
		integer, dimension(:), allocatable	:: ivSpecificDate
		real, dimension(:), allocatable		:: rvSpecificDate
	contains
		procedure :: parse		=> parseProfile
	end type ProfileType
	
contains

	function parseProfile(this, sString) result(iRetCode)
	
		! Routine arguments
		class(ProfileType), intent(in)	:: this
		character(len=*), intent(in)	:: sString
		integer							:: iRetCode
		
		! Locals
		integer		:: iNumTokens
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Count tokens (at least one must exist)
		iNumTokens = CountCharacterOccurrences(sString, ',')
		
	end function parseProfile
	
	! *********************
	! * Internal routines *
	! *********************
	
	function CountCharacterOccurrences(sString, cChar) result(iCount)
	
		! Routine arguments
		character(len=*), intent(in)	:: sString
		character, intent(in)			:: cChar
		integer							:: iCount
		
		! Locals
		integer	:: i
		
		! Check first substring position exists
		iCount = 0
		do i = 1, len_trim(sString)
			if(sString(i:i) == cChar) iCount = iCount + 1
		end do
		if(iCount <= 0) then
			if(sString == " ") then
				iCount = 0	! Blank string: no tokens, by convention
			else
				iCount = 1	! No separating substrings, non-empty string: one token only
			end if
		else	! Normal case: at least one token, count how many actually
			iCount = iCount + 1	! Number of tokens exceeds number of separators by one
		end if
		
	end function CountCharacterOccurrences

	
	function SplitToken(sString, cChar, svToken) result(iRetCode)
	
		! Routine arguments
		character(len=*), intent(in)								:: sString
		character, intent(in)										:: cChar
		character(len=256), dimension(:), allocatable, intent(out)	:: svToken
		integer														:: iRetCode
		
		! Locals
		integer	:: i, j
		integer	:: iCount
		integer	:: iToken
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check first substring position exists
		iCount = 0
		do i = 1, len_trim(sString)
			if(sString(i:i) == cChar) iCount = iCount + 1
		end do
		if(iCount <= 0) then
			if(sString == " ") then
				iCount = 0	! Blank string: no tokens, by convention
			else
				iCount = 1	! No separating substrings, non-empty string: one token only
			end if
		else	! Normal case: at least one token, count how many actually
			iCount = iCount + 1	! Number of tokens exceeds number of separators by one
		end if
		
		! Reserve workspace
		if(iCount <= 0) then
			iRetCode = 1
			return
		end if
		if(allocated(svToken)) deallocate(svToken)
		allocate(svToken(iCount))
		
		! Iterate over string, and save tokens as they are met
		if(iCount == 1) then
			svToken(1) = sString
		else
			iToken = 0
			i = 1
			do j = 1, len_trim(sString)
				if(sString(j:j) == cChar) then
					iToken = iToken + 1
					svToken(iToken) = sString(i:j-1)
					i = j + 1
				end if
			end do
			iToken = iToken + 1
			svToken(iToken) = sString(i:)
		end if
		
	end function SplitToken

end module ModulationProfiles
