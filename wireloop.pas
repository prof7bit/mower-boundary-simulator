unit WireLoop;

{$mode ObjFPC}{$H+}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}
{$optimization fastmath}

interface

uses
  Classes, SysUtils, Vectors, Math, DebugPrint;

type
  { TPlotPlane
    Provides all data amd functions for transformation
    between screen coordinates and model coordinates }

  TPlotPlane = class
  private
    FX0: Double;
    FY0: Double;
    FXMax: Double;
    FYMax: Double;
    FScreenRect: TRect;
    FPlotScale: Double;
    FPlotOffsX: Double;
    FPlotOffsY: Double;
  public
    procedure SetModelSize(Segments: TVectorArray);
    procedure SetScreenRect(Size: TRect);
    function Width: Double;
    function Height: Double;
    function ScreenToVector(X, Y: Integer): TVector;
    function VectorToScreen(V: TVector): TPoint;
    property Scale: Double read FPlotScale;
    property X0: Double read FX0;
    property XMax: Double read FXMax;
    property Y0: Double read FY0;
    property YMax: Double read FYMax;
  end;

  { TWireLoop }

  TWireLoop = class
  private
    FResoluton: Double;
    FSegments: TVectorArray;
    FPlotPlane: TPlotPlane;
    FResolution: Double;
    FCurrPos: TVectorArray;
    FCurrVect: TVectorArray;
    FBoost: Double;
    FSensorHeight: Double;
    procedure CreateCurrentPointsList;
    procedure SetResoluton(AValue: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadLoop(Filename: String);
    property Segments: TVectorArray read FSegments;
    property PlotPlane: TPlotPlane read FPlotPlane;
    function CalcFieldAt(P2: TVector): TVector;
    property Resoluton: Double read FResoluton write SetResoluton;
    property SensorHeight: Double read FSensorHeight write FSensorHeight;
    function WireLength: Double;
  end;


implementation

{ TPlotPlane }

function TPlotPlane.Width: Double;
begin
  Result := FXMax - FX0;
end;

function TPlotPlane.Height: Double;
begin
  Result := FYMax - FY0;
end;

procedure TPlotPlane.SetModelSize(Segments: TVectorArray);
var
  S: TVector;
  A: TVector;
begin
  FX0 := 0;
  FY0 := 0;
  FXMax := 0;
  FYMax := 0;
  A := Vector(0, 0, 0);
  for S in Segments do begin
    A.Add(S);
    if A.X < FX0 then
      FX0 := A.X;
    if A.X > FXMax then
      FXMax := A.X;
    if A.Y < FY0 then
      FY0 := A.Y;
    if A.Y > FYMax then
      FYMax := A.Y;
  end;
end;

procedure TPlotPlane.SetScreenRect(Size: TRect);
const
  BORDER_CM = 50;
  BORDER_PX = 100;
var
  W, H: Double;
  SX, SY: Double;

begin
  W := Width + BORDER_CM;
  H := Height + BORDER_CM;

  FScreenRect := Size;
  SX := Size.Width / W;
  SY := Size.Height / H;
  FPlotScale := Math.Min(SX, SY);

  if Size.Height - Height * FPlotScale < BORDER_PX then
    FPlotScale := (Size.Height - BORDER_PX) / Height;

  FPlotOffsX := (Size.Width / FPlotScale - Width) / 2;
  FPlotOffsY := (Size.Height / FPlotScale - Height) / 2;
end;

function TPlotPlane.ScreenToVector(X, Y: Integer): TVector;
begin
  Result.X := FX0 + (X - FScreenRect.Left) / FPlotScale - FPlotOffsX;
  Result.Y := FY0 + (FScreenRect.Bottom - Y) / FPlotScale - FPlotOffsY;
  Result.Z := 0;
end;

function TPlotPlane.VectorToScreen(V: TVector): TPoint;
begin
  Result.X := Round(FPlotScale * (V.X - FX0 + FPlotOffsX) + FScreenRect.Left);
  Result.Y := Round(FScreenRect.Bottom - FPlotScale * (V.Y - FY0 + FPlotOffsY));
end;

{ TWireLoop }

procedure TWireLoop.CreateCurrentPointsList;
var
  Seg, Vec, HalfVec, Pos: TVector;
  NumPoints, I: Integer;
begin
  FCurrPos := [];
  FCurrVect := [];
  Pos := Vector(0, 0, 0);
  for Seg in FSegments do begin
    NumPoints := Math.Ceil(Mag(Seg) / FResolution);
    Vec := Seg / NumPoints;
    HalfVec := Vec / 2;
    for I := 0 to NumPoints - 1 do begin
      FCurrPos += [Pos + HalfVec];
      FCurrVect += [Vec];
      Pos.Add(Vec);
    end;
  end;
end;

procedure TWireLoop.SetResoluton(AValue: Double);
begin
  if FResoluton = AValue then Exit;
  FResoluton := AValue;
  CreateCurrentPointsList;
end;

function TWireLoop.CalcFieldAt(P2: TVector): TVector;
var
  I: Integer;
  P, D, F: TVector;
  Dist, DistNorm: TVector;
  DistMag: Double;
  DistMagCube: Double;
begin
  // Biot-Savart
  Result := Vector(0, 0, 0);
  for I := 0 to Length(FCurrPos) - 1 do begin
    P := FCurrPos[I];
    D := FCurrVect[I];
    Dist := P2 - P;
    DistMag := Mag(Dist);
    DistMagCube := DistMag * DistMag * DistMag;
    F := (D * Dist) / DistMagCube;
    Result.Add(F);
  end;
  Result := Result * FBoost * 1000;
end;

function TWireLoop.WireLength: Double;
var
  S: TVector;
begin
  Result := 0;
  for S in FSegments do
    Result += Mag(S);
end;

constructor TWireLoop.Create;
begin
  FPlotPlane := TPlotPlane.Create;
end;

destructor TWireLoop.Destroy;
begin
  FreeAndNil(FPlotPlane);
  inherited Destroy;
end;

procedure TWireLoop.LoadLoop(Filename: String);
var
  SL: TStringList;
  Line: String;
  Parts: TStringArray;
  Parts2: TStringArray;
  Part: String;
  X, Y: Double;
  FS: TFormatSettings;

  function ToDouble(S: String; D: Double): Double;
  begin
    Result := StrToFloatDef(S, D, FS);
  end;

begin
  if not FileExists(FileName) then
    exit;

  FResolution := 1;
  FSensorHeight := 10;
  FBoost := 100;

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := ',';
  SL := TStringList.Create;
  SL.LoadFromFile(FileName);

  FSegments := [];
  for Line in SL do begin
    Parts := Line.Split(' ');
    Parts2 := [];
    for Part in Parts do
      if Part <> '' then
        Parts2 += [Trim(Part)];
    if Length(Parts2) > 0 then begin
      if (Parts2[0] = 'wire') and (Length(Parts2) = 3) then begin
        X := ToDouble(Parts2[1], 0);
        Y := ToDouble(Parts2[2], 0);
        FSegments += [VectorXY(X, Y)];
      end;
      if (Parts2[0] = 'boost') and (Length(Parts2) = 2) then begin
        FBoost := ToDouble(Parts2[1], FBoost);
      end;
      if (Parts2[0] = 'height') and (Length(Parts2) = 2) then begin
        FSensorHeight := ToDouble(Parts2[1], FSensorHeight);
      end;
    end;
  end;

  FPlotPlane.SetModelSize(FSegments);
  CreateCurrentPointsList;
end;

end.

