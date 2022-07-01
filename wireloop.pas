unit WireLoop;

{$mode ObjFPC}{$H+}
{$ModeSwitch arrayoperators}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Vectors, Math;

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
    FCurrPoints: TVectorArray;
    FCurrDirs: TVectorArray;
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
var
  W, H: Double;
  SX, SY: Double;
begin
  W := Width + 50;
  H := Height + 50;

  FScreenRect := Size;
  SX := Size.Width / W;
  SY := Size.Height / H;
  FPlotScale := Math.Min(SX, SY);

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
  Seg, Dir, Abs: TVector;
  NumPoints, I: Integer;
begin
  FCurrPoints := [];
  FCurrDirs := [];
  Abs := Vector(0, 0, 0);
  for Seg in FSegments do begin
    NumPoints := Math.Ceil(Mag(Seg) / FResolution);
    Dir := Seg / NumPoints;
    for I := 0 to NumPoints - 1 do begin
      Abs.Add(Dir);
      FCurrPoints += [Abs];
      FCurrDirs += [Dir];
    end;
  end;
  WriteLn(Length(FCurrPoints));
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
begin
  Result := Vector(0, 0, 0);
  for I := 0 to Length(FCurrPoints) - 1 do begin
    P := FCurrPoints[I];
    D := FCurrDirs[I];
    Dist := P - P2;
    DistMag := Mag(Dist);
    DistNorm := Dist / DistMag;
    F := (DistNorm * D) / (DistMag * DistMag);
    Result.Add(F);
  end;
  Result := Result * FBoost;
end;

constructor TWireLoop.Create;
begin
  FResolution := 0.5;
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
begin
  if not FileExists(FileName) then
    exit;

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
        X := StrToFloatDef(Parts2[1], 0);
        Y := StrToFloatDef(Parts2[2], 0);
        FSegments += [VectorXY(X, Y)];
      end;
      if (Parts2[0] = 'boost') and (Length(Parts2) = 2) then begin
        FBoost := StrToFloatDef(Parts2[1], 1);
      end;
      if (Parts2[0] = 'height') and (Length(Parts2) = 2) then begin
        FSensorHeight := StrToFloatDef(Parts2[1], 1);
      end;
    end;
  end;

  FPlotPlane.SetModelSize(FSegments);
  CreateCurrentPointsList;
end;

end.

