

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module

                     MAP
                       INCLUDE('DICTIONARYEDITOR033.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ParseFontFromLong    PROCEDURE  (string pFontHandle,LOGFONT pFontGrp) ! Declare Procedure
RetVal      Long
FontHandle  Long
FontGrp     LIKE(LOGFONT)
Fld         ANY
FldName     String(30)
x           Long
MyDCT       dwrDctParser
st          StringTheory

  CODE
    st.SetValue(pFontHandle,True)
    st.KeepChars('0123456789')
    MyDCT.Trace('[ParseFontFromLong]pFontHandle['& st.GetValue() & ']')
    FontHandle = st.GetValue() !pFontHandle
    RetVal = Dct_GetObjectA(FontHandle, SIZE(FontGrp), ADDRESS(FontGrp))
    MyDCT.Trace('<9>RetVal['& RetVal &']<9>' & Clip(FormatMessage(Dct_GetLastError())))
    !If RetVal <> 0
        LOOP
            x+=1
            Fld &= WHAT(FontGrp,x)
            If Fld &= Null; Break END
            FldName = WHO(FontGrp,x)
            MyDCT.Trace(Clip(FldName) & ' ' & Clip(Fld))
        END
    !ELSE
    !    MyDCT.Trace(Clip(FormatMessage(Dct_GetLastError())))
    !End
