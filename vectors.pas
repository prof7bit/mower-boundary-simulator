unit Vectors;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$optimization fastmath}

interface

uses
  Classes, SysUtils, Math;

type

  { TVector }

  TVector = record
    X: Double;
    Y: Double;
    Z: Double;
    procedure Add(B: TVector);
  end;

  TVectorArray = array of TVector;

  operator + (A, B: TVector): TVector;
  operator - (A, B: TVector): TVector;
  operator * (A, B: TVector): TVector;  // cross product
  operator * (A: Double; B: TVector): TVector;
  operator * (A: TVector; B: Double): TVector;
  operator / (A: TVector; B: Double): TVector;
  function Ang(A, B: TVector): Double;
  function Mag(A: TVector): Double;
  function Norm(A: TVector): TVector;
  function MagSquared(A: TVector): Double;
  function Vector(X, Y, Z: Double): TVector;
  function VectorXY(X, Y: Double): TVector;


implementation

operator + (A, B: TVector): TVector;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

operator - (A, B: TVector): TVector;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

operator * (A, B: TVector): TVector;
begin
  Result.X := A.Y * B.Z - A.Z * B.Y;
  Result.Y := A.Z * B.X - A.X * B.Z;
  Result.Z := A.X * B.Y - A.Y * B.X;
end;

operator * (A: Double; B: TVector): TVector;
begin
  Result.X := A * B.X;
  Result.Y := A * B.Y;
  Result.Z := A * B.Z;
end;

operator * (A: TVector; B: Double): TVector;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
end;

operator / (A: TVector; B: Double): TVector;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
end;

function Mag(A: TVector): Double;
begin
  Result := Sqrt(A.X * A.X + A.Y * A.Y + A.Z * A.Z);
end;

function Norm(A: TVector): TVector;
begin
  Result := A / Mag(A);
end;

function MagSquared(A: TVector): Double;
begin
  Result := A.X * A.X + A.Y * A.Y + A.Z * A.Z;
end;

function Vector(X, Y, Z: Double): TVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function VectorXY(X, Y: Double): TVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := 0;
end;


function Ang(A, B: TVector): Double;
var
  MagCross: Double;
  ProdMag: Double;
begin
  MagCross := Mag(A * B);
  ProdMag := Mag(A) * Mag(B);
  if ProdMag <> 0 then
    Result := ArcSin(MagCross / ProdMag)
  else
    Result := 0;
end;

{ TVector }

procedure TVector.Add(B: TVector);
begin
  Self.X += B.X;
  Self.Y += B.Y;
  Self.Z += B.Z;
end;

{ TVector }


end.

