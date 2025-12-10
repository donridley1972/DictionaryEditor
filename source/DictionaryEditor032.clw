

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module

                     MAP
                       INCLUDE('DICTIONARYEDITOR032.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
FormatMessage        PROCEDURE  (long pError)              ! Declare Procedure
winErrMessage       cstring(255)
errMessage          string(255)
numChars            ulong

  CODE
    numChars = Dct_FormatMessage(001000h + 000200h, 0, pError, 0, winErrMessage, 255, 0)
    errMessage = winErrMessage
    Return(errMessage)
