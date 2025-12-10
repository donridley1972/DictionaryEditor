  MEMBER('DictionaryEditor.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
DictionaryEditor_BC0:DctInit    PROCEDURE()
DictionaryEditor_BC0:DctKill    PROCEDURE()
DictionaryEditor_BC0:FilesInit  PROCEDURE()
  END

Hide:Access:TagSet_  CLASS(FileManager),TYPE               ! FileManager for TagSet_
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TagSet_  CLASS(RelationManager),TYPE           ! RelationManager for TagSet_
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:TagFile_ CLASS(FileManager),TYPE               ! FileManager for TagFile_
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TagFile_ CLASS(RelationManager),TYPE           ! RelationManager for TagFile_
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:TagFilePos_ CLASS(FileManager),TYPE            ! FileManager for TagFilePos_
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TagFilePos_ CLASS(RelationManager),TYPE        ! RelationManager for TagFilePos_
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Dictionaries CLASS(FileManager),TYPE           ! FileManager for Dictionaries
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Dictionaries CLASS(RelationManager),TYPE       ! RelationManager for Dictionaries
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Tables   CLASS(FileManager),TYPE               ! FileManager for Tables
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Tables   CLASS(RelationManager),TYPE           ! RelationManager for Tables
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:DctVersions CLASS(FileManager),TYPE            ! FileManager for DctVersions
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:DctVersions CLASS(RelationManager),TYPE        ! RelationManager for DctVersions
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Options  CLASS(FileManager),TYPE               ! FileManager for Options
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Options  CLASS(RelationManager),TYPE           ! RelationManager for Options
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Fields   CLASS(FileManager),TYPE               ! FileManager for Fields
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Fields   CLASS(RelationManager),TYPE           ! RelationManager for Fields
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:WindowControls CLASS(FileManager),TYPE         ! FileManager for WindowControls
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:WindowControls CLASS(RelationManager),TYPE     ! RelationManager for WindowControls
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Lines    CLASS(FileManager),TYPE               ! FileManager for Lines
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Lines    CLASS(RelationManager),TYPE           ! RelationManager for Lines
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Keys     CLASS(FileManager),TYPE               ! FileManager for Keys
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Keys     CLASS(RelationManager),TYPE           ! RelationManager for Keys
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Validities CLASS(FileManager),TYPE             ! FileManager for Validities
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Validities CLASS(RelationManager),TYPE         ! RelationManager for Validities
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Relations CLASS(FileManager),TYPE              ! FileManager for Relations
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Relations CLASS(RelationManager),TYPE          ! RelationManager for Relations
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:ForeignMappings CLASS(FileManager),TYPE        ! FileManager for ForeignMappings
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:ForeignMappings CLASS(RelationManager),TYPE    ! RelationManager for ForeignMappings
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:PrimaryMappings CLASS(FileManager),TYPE        ! FileManager for PrimaryMappings
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:PrimaryMappings CLASS(RelationManager),TYPE    ! RelationManager for PrimaryMappings
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:KitchenSink CLASS(FileManager),TYPE            ! FileManager for KitchenSink
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:KitchenSink CLASS(RelationManager),TYPE        ! RelationManager for KitchenSink
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Triggers CLASS(FileManager),TYPE               ! FileManager for Triggers
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Triggers CLASS(RelationManager),TYPE           ! RelationManager for Triggers
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:DctCode  CLASS(FileManager),TYPE               ! FileManager for DctCode
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:DctCode  CLASS(RelationManager),TYPE           ! RelationManager for DctCode
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:DataTypesLkUp CLASS(FileManager),TYPE          ! FileManager for DataTypesLkUp
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:DataTypesLkUp CLASS(RelationManager),TYPE      ! RelationManager for DataTypesLkUp
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:DriversLkUp CLASS(FileManager),TYPE            ! FileManager for DriversLkUp
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:DriversLkUp CLASS(RelationManager),TYPE        ! RelationManager for DriversLkUp
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:TagSet_ &Hide:Access:TagSet_,AUTO,THREAD
_Hide:Relate:TagSet_ &Hide:Relate:TagSet_,AUTO,THREAD
_Hide:Access:TagFile_ &Hide:Access:TagFile_,AUTO,THREAD
_Hide:Relate:TagFile_ &Hide:Relate:TagFile_,AUTO,THREAD
_Hide:Access:TagFilePos_ &Hide:Access:TagFilePos_,AUTO,THREAD
_Hide:Relate:TagFilePos_ &Hide:Relate:TagFilePos_,AUTO,THREAD
_Hide:Access:Dictionaries &Hide:Access:Dictionaries,AUTO,THREAD
_Hide:Relate:Dictionaries &Hide:Relate:Dictionaries,AUTO,THREAD
_Hide:Access:Tables  &Hide:Access:Tables,AUTO,THREAD
_Hide:Relate:Tables  &Hide:Relate:Tables,AUTO,THREAD
_Hide:Access:DctVersions &Hide:Access:DctVersions,AUTO,THREAD
_Hide:Relate:DctVersions &Hide:Relate:DctVersions,AUTO,THREAD
_Hide:Access:Options &Hide:Access:Options,AUTO,THREAD
_Hide:Relate:Options &Hide:Relate:Options,AUTO,THREAD
_Hide:Access:Fields  &Hide:Access:Fields,AUTO,THREAD
_Hide:Relate:Fields  &Hide:Relate:Fields,AUTO,THREAD
_Hide:Access:WindowControls &Hide:Access:WindowControls,AUTO,THREAD
_Hide:Relate:WindowControls &Hide:Relate:WindowControls,AUTO,THREAD
_Hide:Access:Lines   &Hide:Access:Lines,AUTO,THREAD
_Hide:Relate:Lines   &Hide:Relate:Lines,AUTO,THREAD
_Hide:Access:Keys    &Hide:Access:Keys,AUTO,THREAD
_Hide:Relate:Keys    &Hide:Relate:Keys,AUTO,THREAD
_Hide:Access:Validities &Hide:Access:Validities,AUTO,THREAD
_Hide:Relate:Validities &Hide:Relate:Validities,AUTO,THREAD
_Hide:Access:Relations &Hide:Access:Relations,AUTO,THREAD
_Hide:Relate:Relations &Hide:Relate:Relations,AUTO,THREAD
_Hide:Access:ForeignMappings &Hide:Access:ForeignMappings,AUTO,THREAD
_Hide:Relate:ForeignMappings &Hide:Relate:ForeignMappings,AUTO,THREAD
_Hide:Access:PrimaryMappings &Hide:Access:PrimaryMappings,AUTO,THREAD
_Hide:Relate:PrimaryMappings &Hide:Relate:PrimaryMappings,AUTO,THREAD
_Hide:Access:KitchenSink &Hide:Access:KitchenSink,AUTO,THREAD
_Hide:Relate:KitchenSink &Hide:Relate:KitchenSink,AUTO,THREAD
_Hide:Access:Triggers &Hide:Access:Triggers,AUTO,THREAD
_Hide:Relate:Triggers &Hide:Relate:Triggers,AUTO,THREAD
_Hide:Access:DctCode &Hide:Access:DctCode,AUTO,THREAD
_Hide:Relate:DctCode &Hide:Relate:DctCode,AUTO,THREAD
_Hide:Access:DataTypesLkUp &Hide:Access:DataTypesLkUp,AUTO,THREAD
_Hide:Relate:DataTypesLkUp &Hide:Relate:DataTypesLkUp,AUTO,THREAD
_Hide:Access:DriversLkUp &Hide:Access:DriversLkUp,AUTO,THREAD
_Hide:Relate:DriversLkUp &Hide:Relate:DriversLkUp,AUTO,THREAD


DictionaryEditor_BC0:DctInit PROCEDURE()
  CODE
  _Hide:Access:TagSet_ &= NEW(Hide:Access:TagSet_)
  _Hide:Relate:TagSet_ &= NEW(Hide:Relate:TagSet_)
  _Hide:Access:TagFile_ &= NEW(Hide:Access:TagFile_)
  _Hide:Relate:TagFile_ &= NEW(Hide:Relate:TagFile_)
  _Hide:Access:TagFilePos_ &= NEW(Hide:Access:TagFilePos_)
  _Hide:Relate:TagFilePos_ &= NEW(Hide:Relate:TagFilePos_)
  _Hide:Access:Dictionaries &= NEW(Hide:Access:Dictionaries)
  _Hide:Relate:Dictionaries &= NEW(Hide:Relate:Dictionaries)
  _Hide:Access:Tables &= NEW(Hide:Access:Tables)
  _Hide:Relate:Tables &= NEW(Hide:Relate:Tables)
  _Hide:Access:DctVersions &= NEW(Hide:Access:DctVersions)
  _Hide:Relate:DctVersions &= NEW(Hide:Relate:DctVersions)
  _Hide:Access:Options &= NEW(Hide:Access:Options)
  _Hide:Relate:Options &= NEW(Hide:Relate:Options)
  _Hide:Access:Fields &= NEW(Hide:Access:Fields)
  _Hide:Relate:Fields &= NEW(Hide:Relate:Fields)
  _Hide:Access:WindowControls &= NEW(Hide:Access:WindowControls)
  _Hide:Relate:WindowControls &= NEW(Hide:Relate:WindowControls)
  _Hide:Access:Lines &= NEW(Hide:Access:Lines)
  _Hide:Relate:Lines &= NEW(Hide:Relate:Lines)
  _Hide:Access:Keys &= NEW(Hide:Access:Keys)
  _Hide:Relate:Keys &= NEW(Hide:Relate:Keys)
  _Hide:Access:Validities &= NEW(Hide:Access:Validities)
  _Hide:Relate:Validities &= NEW(Hide:Relate:Validities)
  _Hide:Access:Relations &= NEW(Hide:Access:Relations)
  _Hide:Relate:Relations &= NEW(Hide:Relate:Relations)
  _Hide:Access:ForeignMappings &= NEW(Hide:Access:ForeignMappings)
  _Hide:Relate:ForeignMappings &= NEW(Hide:Relate:ForeignMappings)
  _Hide:Access:PrimaryMappings &= NEW(Hide:Access:PrimaryMappings)
  _Hide:Relate:PrimaryMappings &= NEW(Hide:Relate:PrimaryMappings)
  _Hide:Access:KitchenSink &= NEW(Hide:Access:KitchenSink)
  _Hide:Relate:KitchenSink &= NEW(Hide:Relate:KitchenSink)
  _Hide:Access:Triggers &= NEW(Hide:Access:Triggers)
  _Hide:Relate:Triggers &= NEW(Hide:Relate:Triggers)
  _Hide:Access:DctCode &= NEW(Hide:Access:DctCode)
  _Hide:Relate:DctCode &= NEW(Hide:Relate:DctCode)
  _Hide:Access:DataTypesLkUp &= NEW(Hide:Access:DataTypesLkUp)
  _Hide:Relate:DataTypesLkUp &= NEW(Hide:Relate:DataTypesLkUp)
  _Hide:Access:DriversLkUp &= NEW(Hide:Access:DriversLkUp)
  _Hide:Relate:DriversLkUp &= NEW(Hide:Relate:DriversLkUp)
  Relate:TagSet_ &= _Hide:Relate:TagSet_
  Relate:TagFile_ &= _Hide:Relate:TagFile_
  Relate:TagFilePos_ &= _Hide:Relate:TagFilePos_
  Relate:Dictionaries &= _Hide:Relate:Dictionaries
  Relate:Tables &= _Hide:Relate:Tables
  Relate:DctVersions &= _Hide:Relate:DctVersions
  Relate:Options &= _Hide:Relate:Options
  Relate:Fields &= _Hide:Relate:Fields
  Relate:WindowControls &= _Hide:Relate:WindowControls
  Relate:Lines &= _Hide:Relate:Lines
  Relate:Keys &= _Hide:Relate:Keys
  Relate:Validities &= _Hide:Relate:Validities
  Relate:Relations &= _Hide:Relate:Relations
  Relate:ForeignMappings &= _Hide:Relate:ForeignMappings
  Relate:PrimaryMappings &= _Hide:Relate:PrimaryMappings
  Relate:KitchenSink &= _Hide:Relate:KitchenSink
  Relate:Triggers &= _Hide:Relate:Triggers
  Relate:DctCode &= _Hide:Relate:DctCode
  Relate:DataTypesLkUp &= _Hide:Relate:DataTypesLkUp
  Relate:DriversLkUp &= _Hide:Relate:DriversLkUp


DictionaryEditor_BC0:FilesInit PROCEDURE()
  CODE
  _Hide:Relate:TagSet_.Init
  _Hide:Relate:TagFile_.Init
  _Hide:Relate:TagFilePos_.Init
  _Hide:Relate:Dictionaries.Init
  _Hide:Relate:Tables.Init
  _Hide:Relate:DctVersions.Init
  _Hide:Relate:Options.Init
  _Hide:Relate:Fields.Init
  _Hide:Relate:WindowControls.Init
  _Hide:Relate:Lines.Init
  _Hide:Relate:Keys.Init
  _Hide:Relate:Validities.Init
  _Hide:Relate:Relations.Init
  _Hide:Relate:ForeignMappings.Init
  _Hide:Relate:PrimaryMappings.Init
  _Hide:Relate:KitchenSink.Init
  _Hide:Relate:Triggers.Init
  _Hide:Relate:DctCode.Init
  _Hide:Relate:DataTypesLkUp.Init
  _Hide:Relate:DriversLkUp.Init


DictionaryEditor_BC0:DctKill PROCEDURE()
  CODE
  _Hide:Relate:TagSet_.Kill
  DISPOSE(_Hide:Relate:TagSet_)
  _Hide:Relate:TagFile_.Kill
  DISPOSE(_Hide:Relate:TagFile_)
  _Hide:Relate:TagFilePos_.Kill
  DISPOSE(_Hide:Relate:TagFilePos_)
  _Hide:Relate:Dictionaries.Kill
  DISPOSE(_Hide:Relate:Dictionaries)
  _Hide:Relate:Tables.Kill
  DISPOSE(_Hide:Relate:Tables)
  _Hide:Relate:DctVersions.Kill
  DISPOSE(_Hide:Relate:DctVersions)
  _Hide:Relate:Options.Kill
  DISPOSE(_Hide:Relate:Options)
  _Hide:Relate:Fields.Kill
  DISPOSE(_Hide:Relate:Fields)
  _Hide:Relate:WindowControls.Kill
  DISPOSE(_Hide:Relate:WindowControls)
  _Hide:Relate:Lines.Kill
  DISPOSE(_Hide:Relate:Lines)
  _Hide:Relate:Keys.Kill
  DISPOSE(_Hide:Relate:Keys)
  _Hide:Relate:Validities.Kill
  DISPOSE(_Hide:Relate:Validities)
  _Hide:Relate:Relations.Kill
  DISPOSE(_Hide:Relate:Relations)
  _Hide:Relate:ForeignMappings.Kill
  DISPOSE(_Hide:Relate:ForeignMappings)
  _Hide:Relate:PrimaryMappings.Kill
  DISPOSE(_Hide:Relate:PrimaryMappings)
  _Hide:Relate:KitchenSink.Kill
  DISPOSE(_Hide:Relate:KitchenSink)
  _Hide:Relate:Triggers.Kill
  DISPOSE(_Hide:Relate:Triggers)
  _Hide:Relate:DctCode.Kill
  DISPOSE(_Hide:Relate:DctCode)
  _Hide:Relate:DataTypesLkUp.Kill
  DISPOSE(_Hide:Relate:DataTypesLkUp)
  _Hide:Relate:DriversLkUp.Kill
  DISPOSE(_Hide:Relate:DriversLkUp)


Hide:Relate:TagSet_.Init PROCEDURE()
  CODE
  _Hide:Access:TagSet_.Init
  SELF.Init(Access:TagSet_,1)


Hide:Access:TagSet_.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TS_:Record
  SELF.FileNameValue = 'TagSet_'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TagSet_
  PARENT.Init
  Access:TagSet_ &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:TagSet_.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TagSet_ &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TagSet_.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TagSet_,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TS_:TblKey,'TS_:TblKey',0)
    SELF.AddKey(TS_:UsrTblKey,'TS_:UsrTblKey',0)
    SELF.AddKey(TS_:UsrSourceDateKey,'TS_:UsrSourceDateKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TagSet_.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:TagFile_,RI:CASCADE,RI:CASCADE,TF_:PtrKey)
  SELF.AddRelationLink(TS_:Usr,TF_:Usr)
  SELF.AddRelationLink(TS_:Tbl,TF_:Tbl)

  SELF.AddRelation(Relate:TagFilePos_,RI:CASCADE,RI:CASCADE,TP_:ValKey)
  SELF.AddRelationLink(TS_:Usr,TP_:Usr)
  SELF.AddRelationLink(TS_:Tbl,TP_:Tbl)


Hide:Relate:TagSet_.Kill PROCEDURE

  CODE
  _Hide:Access:TagSet_.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:TagSet_ &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TagSet_)                            ! destroy the file manager


Hide:Relate:TagFile_.Init PROCEDURE()
  CODE
  _Hide:Access:TagFile_.Init
  SELF.Init(Access:TagFile_,1)


Hide:Access:TagFile_.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TF_:Record
  SELF.FileNameValue = 'TagFile_'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TagFile_
  PARENT.Init
  Access:TagFile_ &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:TagFile_.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TagFile_ &= NULL                                  ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TagFile_.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TagFile_,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TF_:PtrKey,'TF_:PtrKey',0)
    SELF.AddKey(TF_:ValKey,'TF_:ValKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TagFile_.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:TagSet_)


Hide:Relate:TagFile_.Kill PROCEDURE

  CODE
  _Hide:Access:TagFile_.Kill                               ! Kill the file manager
  PARENT.Kill
  Relate:TagFile_ &= NULL                                  ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TagFile_)                           ! destroy the file manager


Hide:Relate:TagFilePos_.Init PROCEDURE()
  CODE
  _Hide:Access:TagFilePos_.Init
  SELF.Init(Access:TagFilePos_,1)


Hide:Access:TagFilePos_.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TP_:Record
  SELF.FileNameValue = 'TagFilePos_'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TagFilePos_
  PARENT.Init
  Access:TagFilePos_ &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:TagFilePos_.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TagFilePos_ &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TagFilePos_.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TagFilePos_,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TP_:PosKey,'TP_:PosKey',0)
    SELF.AddKey(TP_:ValKey,'TP_:ValKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TagFilePos_.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:TagSet_)


Hide:Relate:TagFilePos_.Kill PROCEDURE

  CODE
  _Hide:Access:TagFilePos_.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:TagFilePos_ &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TagFilePos_)                        ! destroy the file manager


Hide:Relate:Dictionaries.Init PROCEDURE()
  CODE
  _Hide:Access:Dictionaries.Init
  SELF.Init(Access:Dictionaries,1)


Hide:Access:Dictionaries.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Dct:Record
  SELF.FileNameValue = 'Dictionaries'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Dictionaries
  PARENT.Init
  Access:Dictionaries &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Dictionaries.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Dictionaries &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Dictionaries.PrimeFields PROCEDURE

  CODE
  Dct:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Dictionaries.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Dictionaries,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Dct:PKDctGUIDKey,'Dct:PKDctGUIDKey',0)
    SELF.AddKey(Dct:FKDctNameKey,'Dct:FKDctNameKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Dictionaries.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Tables,RI:CASCADE,RI:None,Tab:FKTabDictionaryGuidKey)
  SELF.AddRelationLink(Dct:GUID,Tab:DictionaryGuid)

  SELF.AddRelation(Relate:DctVersions,RI:CASCADE,RI:None,Ver:FKVerDictionaryGuidKey)
  SELF.AddRelationLink(Dct:GUID,Ver:DictionaryGuid)

  SELF.AddRelation(Relate:Lines,RI:CASCADE,RI:None,Lin:FKLinDictionaryGuidKey)
  SELF.AddRelationLink(Dct:GUID,Lin:DictionaryGuid)

  SELF.AddRelation(Relate:DuplicateKeys,RI:CASCADE,RI:None,DupK:FKDupKDictionaryGuid)
  SELF.AddRelationLink(Dct:GUID,DupK:DictionaryGuid)

  SELF.AddRelation(Relate:DctComments,RI:CASCADE,RI:None,Dcom:FKDcomDictionaryGuidKey)
  SELF.AddRelationLink(Dct:GUID,Dcom:DictionaryGuid)

  SELF.AddRelation(Relate:Options,RI:CASCADE,RI:None,Opt:FKOptDictionaryGuidKey)
  SELF.AddRelationLink(Dct:GUID,Opt:DictionaryGuid)


Hide:Relate:Dictionaries.Kill PROCEDURE

  CODE
  _Hide:Access:Dictionaries.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:Dictionaries &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Dictionaries)                       ! destroy the file manager


Hide:Relate:Tables.Init PROCEDURE()
  CODE
  _Hide:Access:Tables.Init
  SELF.Init(Access:Tables,1)


Hide:Access:Tables.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Tab:Record
  SELF.FileNameValue = 'Tables'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Tables
  PARENT.Init
  Access:Tables &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Tables.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Tables &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Tables.PrimeFields PROCEDURE

  CODE
  Tab:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Tables.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Tables,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Tab:PKTabGuidKey,'Tab:PKTabGuidKey',0)
    SELF.AddKey(Tab:TabTableTypeGuidKey,'Tab:TabTableTypeGuidKey',0)
    SELF.AddKey(Tab:FKTabParentGuidKey,'Tab:FKTabParentGuidKey',0)
    SELF.AddKey(Tab:FKTabDictionaryGuidKey,'Tab:FKTabDictionaryGuidKey',0)
    SELF.AddKey(Tab:FKTabTableGuidKey,'Tab:FKTabTableGuidKey',0)
    SELF.AddKey(Tab:FKTabGuidKey,'Tab:FKTabGuidKey',0)
    SELF.AddKey(Tab:FKTabDctxOrderKey,'Tab:FKTabDctxOrderKey',0)
    SELF.AddKey(Tab:FKTabTableNameKey,'Tab:FKTabTableNameKey',0)
    SELF.AddKey(Tab:FKTabDctGuidAndOrderKey,'Sort table by DCT Guid and DctxOrder value',0)
    SELF.AddKey(Tab:FKTabParentGuidAndOrderKey,'Tab:FKTabParentGuidAndOrderKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Tables.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Dictionaries)

  SELF.AddRelation(Relate:Fields,RI:CASCADE,RI:None,Fld:FKFldParentGuidKey)
  SELF.AddRelationLink(Tab:PKGuid,Fld:ParentGUID)

  SELF.AddRelation(Relate:Keys,RI:CASCADE,RI:None,Key:FKKeyParentGuidKey)
  SELF.AddRelationLink(Tab:PKGuid,Key:ParentGUID)

  SELF.AddRelation(Relate:Triggers,RI:CASCADE,RI:None,Tri:FKTriParentGuidKey)
  SELF.AddRelationLink(Tab:PKGuid,Tri:ParentGUID)

  SELF.AddRelation(Relate:Aliases,RI:CASCADE,RI:None,Ali:FKAliParentGuidKey)
  SELF.AddRelationLink(Tab:PKGuid,Ali:ParentGUID)

  SELF.AddRelation(Relate:Relations,RI:CASCADE,RI:None,Rel:FKRelParentGuidKey)
  SELF.AddRelationLink(Tab:Guid,Rel:ParentGUID)

  SELF.AddRelation(Relate:DuplicateKeys)


Hide:Relate:Tables.Kill PROCEDURE

  CODE
  _Hide:Access:Tables.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:Tables &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Tables)                             ! destroy the file manager


Hide:Relate:DctVersions.Init PROCEDURE()
  CODE
  _Hide:Access:DctVersions.Init
  SELF.Init(Access:DctVersions,1)


Hide:Access:DctVersions.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Ver:Record
  SELF.FileNameValue = 'DctVersions'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= DctVersions
  PARENT.Init
  Access:DctVersions &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:DctVersions.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:DctVersions &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:DctVersions.PrimeFields PROCEDURE

  CODE
  Ver:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:DctVersions.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(DctVersions,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Ver:PKVerGuidKey,'Ver:PKVerGuidKey',0)
    SELF.AddKey(Ver:FKVerParentGuidKey,'Ver:FKVerParentGuidKey',0)
    SELF.AddKey(Ver:FKVerDictionaryGuidKey,'Ver:FKVerDictionaryGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:DctVersions.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Dictionaries)


Hide:Relate:DctVersions.Kill PROCEDURE

  CODE
  _Hide:Access:DctVersions.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:DctVersions &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:DctVersions)                        ! destroy the file manager


Hide:Relate:Options.Init PROCEDURE()
  CODE
  _Hide:Access:Options.Init
  SELF.Init(Access:Options,1)


Hide:Access:Options.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Opt:Record
  SELF.FileNameValue = 'Options'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Options
  PARENT.Init
  Access:Options &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Options.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Options &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Options.PrimeFields PROCEDURE

  CODE
  Opt:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Options.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Options,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Opt:PKOptGuidKey,'Opt:PKOptGuidKey',0)
    SELF.AddKey(Opt:FKOptParentGuidKey,'Opt:FKOptParentGuidKey',0)
    SELF.AddKey(Opt:FKOptDictionaryGuidKey,'Opt:FKOptDictionaryGuidKey',0)
    SELF.AddKey(Opt:FKOptTableGuidKey,'Opt:FKOptTableGuidKey',0)
    SELF.AddKey(Opt:FKOptFieldGuidKey,'Opt:FKOptFieldGuidKey',0)
    SELF.AddKey(Opt:FKOptKeyGuidKey,'Opt:FKOptKeyGuidKey',0)
    SELF.AddKey(Opt:FKOptRelationGuidKey,'Opt:FKOptRelationGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Options.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Dictionaries)


Hide:Relate:Options.Kill PROCEDURE

  CODE
  _Hide:Access:Options.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:Options &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Options)                            ! destroy the file manager


Hide:Relate:Fields.Init PROCEDURE()
  CODE
  _Hide:Access:Fields.Init
  SELF.Init(Access:Fields,1)


Hide:Access:Fields.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Fld:Record
  SELF.FileNameValue = 'Fields'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Fields
  PARENT.Init
  Access:Fields &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Fields.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Fields &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Fields.PrimeFields PROCEDURE

  CODE
  Fld:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  Fld:CtrlType = 'ENTRY'                                   ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Fields.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Fields,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Fld:PKFldGuidKey,'Fld:PKFldGuidKey',0)
    SELF.AddKey(Fld:FKFldParentGuidKey,'Fld:FKFldParentGuidKey',0)
    SELF.AddKey(Fld:FKFldTableGuidKey,'Fld:FKFldTableGuidKey',0)
    SELF.AddKey(Fld:FKFldFieldOrderKey,'Fld:FKFldFieldOrderKey',0)
    SELF.AddKey(Fld:FKFldFieldNameKey,'Fld:FKFldFieldNameKey',0)
    SELF.AddKey(Fld:FKFldDctGuidAndOrderKey,'Fld:FKFldDctGuidAndOrderKey',0)
    SELF.AddKey(Fld:FKFldParentGuidAndOrderKey,'Fld:FKFldParentGuidAndOrderKey',0)
    SELF.AddKey(Fld:FKFldFieldGuidKey,'Fld:FKFldFieldGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Fields.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Tables)

  SELF.AddRelation(Relate:WindowControls,RI:CASCADE,RI:None,Win:FKWinFieldGuidKey)
  SELF.AddRelationLink(Fld:PKGuid,Win:FieldGuid)

  SELF.AddRelation(Relate:Validities,RI:CASCADE,RI:None,Val:FKValFieldGuidKey)
  SELF.AddRelationLink(Fld:PKGuid,Val:FieldGuid)

  SELF.AddRelation(Relate:Keys,RI:CASCADE,RI:None,Key:FKKeyParentGuidKey)
  SELF.AddRelationLink(Fld:ParentGUID,Key:ParentGUID)

  SELF.AddRelation(Relate:TreeMemTable)


Hide:Relate:Fields.Kill PROCEDURE

  CODE
  _Hide:Access:Fields.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:Fields &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Fields)                             ! destroy the file manager


Hide:Relate:WindowControls.Init PROCEDURE()
  CODE
  _Hide:Access:WindowControls.Init
  SELF.Init(Access:WindowControls,1)


Hide:Access:WindowControls.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Win:Record
  SELF.FileNameValue = 'WindowControls'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= WindowControls
  PARENT.Init
  Access:WindowControls &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:WindowControls.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:WindowControls &= NULL                            ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:WindowControls.PrimeFields PROCEDURE

  CODE
  Win:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:WindowControls.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(WindowControls,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Win:PKWinGuidKey,'Win:PKWinGuidKey',0)
    SELF.AddKey(Win:FKWinParentGUIDKey,'Win:FKWinParentGUIDKey',0)
    SELF.AddKey(Win:FKWinTableGuidKey,'Win:FKWinTableGuidKey',0)
    SELF.AddKey(Win:FKWinFieldGuidKey,'Win:FKWinFieldGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:WindowControls.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Fields)

  SELF.AddRelation(Relate:Lines,RI:CASCADE,RI:None,Lin:FKLinWinControlGuidKey)
  SELF.AddRelationLink(Win:GUID,Lin:WinControlGuid)


Hide:Relate:WindowControls.Kill PROCEDURE

  CODE
  _Hide:Access:WindowControls.Kill                         ! Kill the file manager
  PARENT.Kill
  Relate:WindowControls &= NULL                            ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:WindowControls)                     ! destroy the file manager


Hide:Relate:Lines.Init PROCEDURE()
  CODE
  _Hide:Access:Lines.Init
  SELF.Init(Access:Lines,1)


Hide:Access:Lines.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Lin:Record
  SELF.FileNameValue = 'Lines'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Lines
  PARENT.Init
  Access:Lines &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Lines.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Lines &= NULL                                     ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Lines.PrimeFields PROCEDURE

  CODE
  Lin:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Lines.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Lines,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Lin:PKLinGuidKey,'Lin:PKLinGuidKey',0)
    SELF.AddKey(Lin:FKLinParentGuidKey,'Lin:FKLinParentGuidKey',0)
    SELF.AddKey(Lin:FKLinDictionaryGuidKey,'Lin:FKLinDictionaryGuidKey',0)
    SELF.AddKey(Lin:FKLinWinControlGuidKey,'Lin:FKLinWinControlGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Lines.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:WindowControls)

  SELF.AddRelation(Relate:Dictionaries)


Hide:Relate:Lines.Kill PROCEDURE

  CODE
  _Hide:Access:Lines.Kill                                  ! Kill the file manager
  PARENT.Kill
  Relate:Lines &= NULL                                     ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Lines)                              ! destroy the file manager


Hide:Relate:Keys.Init PROCEDURE()
  CODE
  _Hide:Access:Keys.Init
  SELF.Init(Access:Keys,1)


Hide:Access:Keys.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Key:Record
  SELF.FileNameValue = 'Keys'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Keys
  PARENT.Init
  Access:Keys &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Keys.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Keys &= NULL                                      ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Keys.PrimeFields PROCEDURE

  CODE
  Key:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Keys.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Keys,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Key:PKKeyGuidKey,'Key:PKKeyGuidKey',0)
    SELF.AddKey(Key:FKKeyDictionaryGuidKey,'Key:FKKeyDictionaryGuidKey',0)
    SELF.AddKey(Key:FKKeyParentGuidKey,'Key:FKKeyParentGuidKey',0)
    SELF.AddKey(Key:FKKeyTableGuidKey,'Key:FKKeyTableGuidKey',0)
    SELF.AddKey(Key:FKKeyNameKey,'Key:FKKeyNameKey',0)
    SELF.AddKey(Key:FKKeyOrderKey,'Key:FKKeyOrderKey',0)
    SELF.AddKey(Key:FKKeyParentGuidAndOrderKey,'Key:FKKeyParentGuidAndOrderKey',0)
    SELF.AddKey(Key:FKKeyKeyGuidKey,'Key:FKKeyKeyGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Keys.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Tables)

  SELF.AddRelation(Relate:Fields)

  SELF.AddRelation(Relate:TreeMemTable)


Hide:Relate:Keys.Kill PROCEDURE

  CODE
  _Hide:Access:Keys.Kill                                   ! Kill the file manager
  PARENT.Kill
  Relate:Keys &= NULL                                      ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Keys)                               ! destroy the file manager


Hide:Relate:Validities.Init PROCEDURE()
  CODE
  _Hide:Access:Validities.Init
  SELF.Init(Access:Validities,1)


Hide:Access:Validities.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Val:Record
  SELF.FileNameValue = 'Validities'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Validities
  PARENT.Init
  Access:Validities &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Validities.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Validities &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Validities.PrimeFields PROCEDURE

  CODE
  Val:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Validities.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Validities,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Val:PKValGuidKey,'Val:PKValGuidKey',0)
    SELF.AddKey(Val:FKValParentGuidKey,'Val:FKValParentGuidKey',0)
    SELF.AddKey(Val:FKValFieldGuidKey,'Val:FKValFieldGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Validities.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Fields)


Hide:Relate:Validities.Kill PROCEDURE

  CODE
  _Hide:Access:Validities.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:Validities &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Validities)                         ! destroy the file manager


Hide:Relate:Relations.Init PROCEDURE()
  CODE
  _Hide:Access:Relations.Init
  SELF.Init(Access:Relations,1)


Hide:Access:Relations.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Rel:Record
  SELF.FileNameValue = 'Relations'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Relations
  PARENT.Init
  Access:Relations &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Relations.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Relations &= NULL                                 ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Relations.PrimeFields PROCEDURE

  CODE
  Rel:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Relations.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Relations,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Rel:PKRelGuidKey,'Rel:PKRelGuidKey',0)
    SELF.AddKey(Rel:FKRelParentGuidKey,'Rel:FKRelParentGuidKey',0)
    SELF.AddKey(Rel:FKRelPrimaryTableKey,'Rel:FKRelPrimaryTableKey',0)
    SELF.AddKey(Rel:FKRelForeignTableKey,'Rel:FKRelForeignTableKey',0)
    SELF.AddKey(Rel:FKRelDctxOrderKey,'Rel:FKRelDctxOrderKey',0)
    SELF.AddKey(Rel:FKRelPrimaryKeyKey,'Rel:FKRelPrimaryKeyKey',0)
    SELF.AddKey(Rel:FKRelForeignKeyKey,'Rel:FKRelForeignKeyKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Relations.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:ForeignMappings,RI:CASCADE,RI:None,For:FKRelRelationGuidKey)
  SELF.AddRelationLink(Rel:PKGuid,For:RelationGuid)

  SELF.AddRelation(Relate:PrimaryMappings,RI:CASCADE,RI:None,Prim:FKPrimRelationGuidKey)
  SELF.AddRelationLink(Rel:PKGuid,Prim:RelationGuid)

  SELF.AddRelation(Relate:Tables)


Hide:Relate:Relations.Kill PROCEDURE

  CODE
  _Hide:Access:Relations.Kill                              ! Kill the file manager
  PARENT.Kill
  Relate:Relations &= NULL                                 ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Relations)                          ! destroy the file manager


Hide:Relate:ForeignMappings.Init PROCEDURE()
  CODE
  _Hide:Access:ForeignMappings.Init
  SELF.Init(Access:ForeignMappings,1)


Hide:Access:ForeignMappings.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= For:Record
  SELF.FileNameValue = 'ForeignMappings'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= ForeignMappings
  PARENT.Init
  Access:ForeignMappings &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:ForeignMappings.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:ForeignMappings &= NULL                           ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:ForeignMappings.PrimeFields PROCEDURE

  CODE
  For:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:ForeignMappings.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(ForeignMappings,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(For:PKForGuidKey,'For:PKForGuidKey',0)
    SELF.AddKey(For:FKForParentGuidKey,'For:FKForParentGuidKey',0)
    SELF.AddKey(For:FKRelRelationGuidKey,'For:FKRelRelationGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:ForeignMappings.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Relations)


Hide:Relate:ForeignMappings.Kill PROCEDURE

  CODE
  _Hide:Access:ForeignMappings.Kill                        ! Kill the file manager
  PARENT.Kill
  Relate:ForeignMappings &= NULL                           ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:ForeignMappings)                    ! destroy the file manager


Hide:Relate:PrimaryMappings.Init PROCEDURE()
  CODE
  _Hide:Access:PrimaryMappings.Init
  SELF.Init(Access:PrimaryMappings,1)


Hide:Access:PrimaryMappings.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Prim:Record
  SELF.FileNameValue = 'PrimaryMappings'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= PrimaryMappings
  PARENT.Init
  Access:PrimaryMappings &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:PrimaryMappings.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:PrimaryMappings &= NULL                           ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:PrimaryMappings.PrimeFields PROCEDURE

  CODE
  Prim:GUID = Glo:st.MakeGuid()                            ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:PrimaryMappings.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(PrimaryMappings,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Prim:PKPrimGuidKey,'Prim:PKPrimGuidKey',0)
    SELF.AddKey(Prim:FKPrimParentGuidKey,'Prim:FKPrimParentGuidKey',0)
    SELF.AddKey(Prim:FKPrimRelationGuidKey,'Prim:FKPrimRelationGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:PrimaryMappings.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Relations)


Hide:Relate:PrimaryMappings.Kill PROCEDURE

  CODE
  _Hide:Access:PrimaryMappings.Kill                        ! Kill the file manager
  PARENT.Kill
  Relate:PrimaryMappings &= NULL                           ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:PrimaryMappings)                    ! destroy the file manager


Hide:Relate:KitchenSink.Init PROCEDURE()
  CODE
  _Hide:Access:KitchenSink.Init
  SELF.Init(Access:KitchenSink,1)


Hide:Access:KitchenSink.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Kit:Record
  SELF.FileNameValue = 'KitchenSink'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= KitchenSink
  PARENT.Init
  Access:KitchenSink &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:KitchenSink.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:KitchenSink &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:KitchenSink.PrimeFields PROCEDURE

  CODE
  Kit:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  Kit:NumericRange = 2                                     ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:KitchenSink.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(KitchenSink,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Kit:PKKitGuidKey,'Kit:PKKitGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:KitchenSink.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 4
    GlobalErrors.SetField('Kit:NumericRange')
    IF NOT INRANGE(Kit:NumericRange,0,100)
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldOutOfRange,'0 .. 100')
      END
    END
  OF 6
    GlobalErrors.SetField('Kit:MustBeInList')
    IF INSTRING(']' & CLIP(Kit:MustBeInList) & '[', ']1[]2[]3[]4[', 1, 1) = 0
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInList,'''One'',''Two'',''Three'',''Four''')
      END
    END
  END
  RETURN ReturnValue


Hide:Relate:KitchenSink.Kill PROCEDURE

  CODE
  _Hide:Access:KitchenSink.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:KitchenSink &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:KitchenSink)                        ! destroy the file manager


Hide:Relate:Triggers.Init PROCEDURE()
  CODE
  _Hide:Access:Triggers.Init
  SELF.Init(Access:Triggers,1)


Hide:Access:Triggers.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Tri:Record
  SELF.FileNameValue = 'Triggers'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Triggers
  PARENT.Init
  Access:Triggers &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Triggers.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Triggers &= NULL                                  ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Triggers.PrimeFields PROCEDURE

  CODE
  Tri:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Triggers.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Triggers,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Tri:PKTriGuidKey,'Tri:PKTriGuidKey',0)
    SELF.AddKey(Tri:FKTriParentGuidKey,'Tri:FKTriParentGuidKey',0)
    SELF.AddKey(Tri:FKTriTableGuidKey,'Tri:FKTriTableGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Triggers.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Tables)

  SELF.AddRelation(Relate:DctCode,RI:CASCADE,RI:None,Cod:FKCodTriggerGuidKey)
  SELF.AddRelationLink(Tri:PKGuid,Cod:TriggerGuid)

  SELF.AddRelation(Relate:TreeMemTable)


Hide:Relate:Triggers.Kill PROCEDURE

  CODE
  _Hide:Access:Triggers.Kill                               ! Kill the file manager
  PARENT.Kill
  Relate:Triggers &= NULL                                  ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Triggers)                           ! destroy the file manager


Hide:Relate:DctCode.Init PROCEDURE()
  CODE
  _Hide:Access:DctCode.Init
  SELF.Init(Access:DctCode,1)


Hide:Access:DctCode.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Cod:Record
  SELF.FileNameValue = 'DctCode'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= DctCode
  PARENT.Init
  Access:DctCode &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:DctCode.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:DctCode &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:DctCode.PrimeFields PROCEDURE

  CODE
  Cod:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:DctCode.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(DctCode,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Cod:PKCodGuidKey,'Cod:PKCodGuidKey',0)
    SELF.AddKey(Cod:FKCodParentGuidKey,'Cod:FKCodParentGuidKey',0)
    SELF.AddKey(Cod:FKCodTriggerGuidKey,'Cod:FKCodTriggerGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:DctCode.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Triggers)


Hide:Relate:DctCode.Kill PROCEDURE

  CODE
  _Hide:Access:DctCode.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:DctCode &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:DctCode)                            ! destroy the file manager


Hide:Relate:DataTypesLkUp.Init PROCEDURE()
  CODE
  _Hide:Access:DataTypesLkUp.Init
  SELF.Init(Access:DataTypesLkUp,1)


Hide:Access:DataTypesLkUp.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Dat:Record
  SELF.FileNameValue = 'DataTypesLkUp'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= DataTypesLkUp
  PARENT.Init
  Access:DataTypesLkUp &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:DataTypesLkUp.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:DataTypesLkUp &= NULL                             ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:DataTypesLkUp.PrimeFields PROCEDURE

  CODE
  Dat:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:DataTypesLkUp.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(DataTypesLkUp,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Dat:PKDatGuidKey,'Dat:PKDatGuidKey',0)
    SELF.AddKey(Dat:FKDatTypeKey,'Dat:FKDatTypeKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:DataTypesLkUp.Kill PROCEDURE

  CODE
  _Hide:Access:DataTypesLkUp.Kill                          ! Kill the file manager
  PARENT.Kill
  Relate:DataTypesLkUp &= NULL                             ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:DataTypesLkUp)                      ! destroy the file manager


Hide:Relate:DriversLkUp.Init PROCEDURE()
  CODE
  _Hide:Access:DriversLkUp.Init
  SELF.Init(Access:DriversLkUp,1)


Hide:Access:DriversLkUp.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Dri:Record
  SELF.FileNameValue = 'DriversLkUp'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= DriversLkUp
  PARENT.Init
  Access:DriversLkUp &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:DriversLkUp.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:DriversLkUp &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:DriversLkUp.PrimeFields PROCEDURE

  CODE
  Dri:GUID = Glo:st.MakeGuid()                             ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:DriversLkUp.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(DriversLkUp,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Dri:PKDriGuidKey,'Dri:PKDriGuidKey',0)
    SELF.AddKey(Dri:FKDriDriverKey,'Dri:FKDriDriverKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:DriversLkUp.Kill PROCEDURE

  CODE
  _Hide:Access:DriversLkUp.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:DriversLkUp &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:DriversLkUp)                        ! destroy the file manager

