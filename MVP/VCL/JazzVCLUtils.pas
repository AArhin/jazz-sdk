unit JazzVCLUtils;

interface

uses
  ComCtrls;

procedure PageControlHideTabs(PageControl: TPageControl);

implementation

procedure PageControlHideTabs(PageControl: TPageControl);
var
  I: Integer;
begin
  with PageControl do
  begin
    for I:= PageCount -1 downto 0 do Pages[I].TabVisible:= False;
    ActivePageIndex:= 0;
  end;
end;

end.
