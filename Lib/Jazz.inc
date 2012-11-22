{.$DEFINE JAZZ_MULTITHREAD} // Make sure that shared items will be exclusive
{.$DEFINE JAZZ_INTL} // Internationalization - if defined convert to/from UTF8

{$IFDEF DEBUG}
  {$DEFINE JAZZ_DEBUG} // Used like assert - remove this to optimize and reduce some validations
{$ENDIF}

{$H+} { Use long strings by default }
{$T-} { Typed @ operator }

{$IFDEF JAZZ_DEBUG}
  {$C+} { Assertions }
{$ENDIF}

{.$DEFINE JAZZ_DEBUG_IDE}
{$DEFINE JAZZ_CHECK_KEYWORDS}

{$IFNDEF JAZZ_DEBUG}
  {$DEFINE JAZZ_RELEASE}
{$ENDIF}

{$IFDEF JAZZ_RELEASE}
  {$B-} { Complete Boolean Evaluation }
  {$C-} { Assertions }
  {$Q-} { Overflow checking }
  {$R-} { Range-Checking }
  {$D-} { Debug information Off }
  {$L-} { Local symbol information Off }
{$ENDIF}

{ Borland Delphi 6 }
{$IFDEF VER140}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
{$ENDIF}

{ Borland Delphi 7 }
{$IFDEF VER150}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
{$ENDIF}

{ Delphi 9 - 2005 }
{$IFDEF VER170}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI9}
{$ENDIF}

{ Delphi 10 - 2006 }
{$IFDEF VER180}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI10}
{$ENDIF}

{ Delphi 11 - 2007}
{$IFDEF VER185}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI11}
{$ENDIF}

{ Delphi 12 - 2009 }
{$IFDEF VER200}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI12}
  {$DEFINE D2009}
{$ENDIF}

{$IFDEF DELPHI6}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$IFNDEF DELPHI7}
    {$UNDEF JAZZ_CHECK_KEYWORDS}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI7}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$IFDEF DELPHI8}
  {$DEFINE BDS_IDE}
{$ENDIF}

{$IFDEF DELPHI9}
  {$UNSAFECODE OFF}
{$ENDIF}

{$IFDEF DELPHI12}
  {$DEFINE JAZZ_INTL}
  {$STRINGCHECKS OFF}
{$ENDIF}

