{$DEFINE DLL}

{$WARNINGS OFF}
{$IFDEF RELEASE}
  {$UNDEF DEBUG}
{$ENDIF}

{$IFDEF VER90}      // Delphi 2.0
  {$DEFINE Delphi2_UP}
{$ENDIF}

{$IFDEF Ver100}      // Delphi 3.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
{$ENDIF}

{$IFDEF VER120}      // Delphi 4.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
{$ENDIF}

{$IFDEF VER130}      // Delphi 5.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
{$ENDIF}

{$IFDEF VER140}      // Delphi 6.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$LIBSUFFIX '6'}
{$ENDIF}

{$IFDEF VER150}      // Delphi 7.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$LIBSUFFIX '7'}
{$ENDIF}

{$IFDEF VER170}      // Delphi 9.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$LIBSUFFIX '9'}
{$ENDIF}

{$IFDEF VER180}      // Delphi 10.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$LIBSUFFIX '10'}
{$ENDIF}

{$IFDEF VER185}      // Delphi 11.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$DEFINE Delphi11_UP}
  {$LIBSUFFIX '11'}
{$ENDIF}

{$IFDEF VER190}      // Delphi 11.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$DEFINE Delphi11_UP}
  {$LIBSUFFIX '11'}
{$ENDIF}

{$IFDEF VER200}      // Delphi 12.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$DEFINE Delphi11_UP}
  {$DEFINE Delphi12_UP}
  {$LIBSUFFIX '12'}
{$ENDIF}

{$IFDEF VER210}      // Delphi 14.0
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$DEFINE Delphi11_UP}
  {$DEFINE Delphi12_UP}
  {$DEFINE Delphi14_UP}
  {$LIBSUFFIX '14'}
{$ENDIF}

{$IFDEF VER220}      // Delphi 15.0  (XE)
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$DEFINE Delphi11_UP}
  {$DEFINE Delphi12_UP}
  {$DEFINE Delphi14_UP}
  {$DEFINE Delphi15_UP}
  {$LIBSUFFIX '15'}
{$ENDIF}

{$IFDEF VER230}      // Delphi 16 (XE2)
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$DEFINE Delphi11_UP}
  {$DEFINE Delphi12_UP}
  {$DEFINE Delphi14_UP}
  {$DEFINE Delphi15_UP}
  {$DEFINE Delphi16_UP}
  {$LIBSUFFIX '16'}
{$ENDIF}

{$IFDEF VER240}      // Delphi 17 (XE3)
  {$DEFINE Delphi2_UP}
  {$DEFINE Delphi3_UP}
  {$DEFINE Delphi4_UP}
  {$DEFINE Delphi5_UP}
  {$DEFINE Delphi6_UP}
  {$DEFINE Delphi7_UP}
  {$DEFINE Delphi9_UP}
  {$DEFINE Delphi10_UP}
  {$DEFINE Delphi11_UP}
  {$DEFINE Delphi12_UP}
  {$DEFINE Delphi14_UP}
  {$DEFINE Delphi15_UP}
  {$DEFINE Delphi16_UP}
  {$DEFINE Delphi17_UP}
  {$LIBSUFFIX '17'}
{$ENDIF}
