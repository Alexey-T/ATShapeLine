unit shapeline_reg;

interface

uses
  SysUtils, Classes, Controls, LResources, shapeline;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TShapeLine]);
end;

initialization
  {$I res/icons.lrs}

end.
