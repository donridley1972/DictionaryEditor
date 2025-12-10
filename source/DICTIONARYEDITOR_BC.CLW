  MEMBER('DictionaryEditor.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('DictionaryEditor_BC0.CLW')
DictionaryEditor_BC0:DctInit             PROCEDURE()
DictionaryEditor_BC0:DctKill             PROCEDURE()
DictionaryEditor_BC0:FilesInit           PROCEDURE()
    END
    MODULE('DictionaryEditor_BC1.CLW')
DictionaryEditor_BC1:DctInit             PROCEDURE()
DictionaryEditor_BC1:DctKill             PROCEDURE()
DictionaryEditor_BC1:FilesInit           PROCEDURE()
    END
  END

DctInit PROCEDURE()
  CODE
  DictionaryEditor_BC0:DctInit
  DictionaryEditor_BC1:DctInit
  DictionaryEditor_BC0:FilesInit
  DictionaryEditor_BC1:FilesInit


DctKill PROCEDURE()
  CODE
  DictionaryEditor_BC0:DctKill
  DictionaryEditor_BC1:DctKill

