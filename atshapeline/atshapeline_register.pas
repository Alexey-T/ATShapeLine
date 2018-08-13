unit atshapeline_register;

interface

uses
  SysUtils, Classes, Controls, LResources, atshapeline;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TShapeLine]);
end;

initialization
  {$I res/icons.lrs}

end.
