unit uGenericDAO;

interface

uses
  RTTI, TypInfo, SysUtils, uAtribEntity;

type
  TGenericDAO = class
  private
    class function GetTableName<T: class>(Obj: T): string;
    class function GetFields(Atributos: TArray<TCustomAttribute>): string;
  public
    class function Insert<T: class>(Obj: T): Boolean;
    class function SelectAll<T: class>(Obj: T): string;
  end;


implementation

class function TGenericDAO.GetFields(Atributos: TArray<TCustomAttribute>): string;
var
  Atributo: TCustomAttribute;
  Fields: string;
begin
  Fields := EmptyStr;

  for Atributo in Atributos do begin
    if Atributo is FieldName then
      Fields := Fields + FieldName(Atributo).Name + ', ';
  end;

  Exit(Copy(Fields, 1, Length(Fields) - 2));
end;

class function TGenericDAO.GetTableName<T>(Obj: T): string;
var
  Contexto: TRttiContext;
  TypObj: TRttiType;
  Atributo: TCustomAttribute;

begin
  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(TObject(Obj).ClassInfo);
  for Atributo in TypObj.GetAttributes do
  begin
    if Atributo is TableName then
      Exit(TableName(Atributo).Name);
  end;
end;

class function TGenericDAO.Insert<T>(Obj: T): Boolean;
var
  Contexto: TRttiContext;
  TypObj: TRttiType;
  Prop: TRttiProperty;
  strInsert, strFields, strValues: String;
  Atributo: TCustomAttribute;

begin
  strInsert := EmptyStr;
  strFields := EmptyStr;
  strValues := EmptyStr;

  strInsert := 'insert into ' + GetTableName(Obj);

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(TObject(Obj).ClassInfo);

  for Prop in TypObj.GetProperties do begin
    for Atributo in Prop.GetAttributes do begin
        if Atributo is FieldName then begin
           strFields := strFields + FieldName(Atributo).Name  + ', ';
           case Prop.GetValue(TObject(Obj)).Kind of

             tkWChar, tkLString, tkWString, tkString, tkChar, tkUString:
               strValues := strValues + QuotedStr(Prop.GetValue(TObject(Obj)).AsString) + ', ';

             tkInteger, tkInt64:
               strValues := strValues + IntToStr(Prop.GetValue(TObject(Obj)).AsInteger) + ', ';

             tkFloat:
               strValues := strValues + FloatToStr(Prop.GetValue(TObject(Obj)).AsExtended) + ', ';

             else
               raise Exception.Create('Type not Supported');
           end;
       end;
     end;
  end;

  strFields := Copy(strFields, 1, Length(strFields) - 1);
  strValues := Copy(strValues, 1, Length(strValues) - 1);
  strInsert := strInsert + ' ( ' + strFields + ' )  values ( ' + strValues + ' )';

  try
    //Executar SQL
    Result := True;
  except
    on e: Exception do
    begin
      raise E.Create('Erro: ' + e.Message);
    end;
  end;

end;

class function TGenericDAO.SelectAll<T>(Obj: T): string;
var
  sql: string;
begin
  Result := 'SELECT T1.* from ' + GetTableName(Obj) + 'T1';
end;

end.
