unit uGenericDAO;

interface

uses
  RTTI, TypInfo, SysUtils, uAtribEntity, System.Generics.Collections, System.StrUtils;

type
  TGenericDAO = class
  private
    class function GetTableName<T: class>(Obj: T): string;
    class function GetFields(Atributos: TArray<TCustomAttribute>): string;
  public
    class function Insert<T: class>(Obj: T): Boolean;
    class function Update<T: class>(Obj: T): Boolean;

    class function SelectAll<T: class>(Obj: T): string;
    class function SelectBasico<T: class>(Obj: T): string;
  end;


implementation

uses
  System.Classes;

const
  cIgualdade = ' = ';

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

  strFields := Copy(strFields, 1, Length(strFields) - 2);
  strValues := Copy(strValues, 1, Length(strValues) - 2);
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

class function TGenericDAO.SelectBasico<T>(Obj: T): string;
var
  Contexto: TRttiContext;
  TypObj: TRttiType;
  Prop: TRttiProperty;
  Atributo: TCustomAttribute;

  scriptSelect: TStringList;
  scriptLigacoes: TStringList;
  strValor: string;
  strCondicaoWhere: string;
  strCampos: string;
  adicionouWhere: Boolean;

  TabelasInseridas: TDictionary<string, string>;
  ApelidoTabelas: TDictionary<string, string>;
  apelido_tab_estrangeira: string;
  tabela_estrangeira: string;
  tabela_principal: string;
  apelido_tab_principal: string;
  x: string;
  tipo_valor: string;

  i: Integer;

begin
  scriptSelect := TStringList.Create;
  scriptLigacoes := TStringList.Create;
  strValor := EmptyStr;
  strCondicaoWhere := EmptyStr;
  strCampos := EmptyStr;
  adicionouWhere := False;

  tabela_principal := GetTableName(Obj);
  apelido_tab_principal := Copy(tabela_principal, 1, 3);
  ApelidoTabelas := TDictionary<string, string>.Create;
  ApelidoTabelas.Add(tabela_principal, apelido_tab_principal);

  TabelasInseridas := TDictionary<string, string>.Create;

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(TObject(Obj).ClassInfo);

  for Prop in TypObj.GetProperties do begin
    for Atributo in Prop.GetAttributes do begin
      if Atributo is PropriedadesCampo then begin
        strValor := EmptyStr;

        if not Prop.GetValue(TObject(Obj)).IsEmpty then
        begin        
          try
            case Prop.GetValue(TObject(Obj)).Kind of
              tkWChar, tkLString, tkWString, tkString, tkChar, tkUString: begin
                tipo_valor := 'string';
                strValor := QuotedStr(Prop.GetValue(TObject(Obj)).AsString);
              end;

              tkInteger, tkInt64: begin
                tipo_valor := 'Integer';
                strValor := IntToStr(Prop.GetValue(TObject(Obj)).AsInteger);
              end;

              tkFloat: begin
                tipo_valor := 'Float';
                strValor := FloatToStr(Prop.GetValue(TObject(Obj)).AsExtended);
              end;

              else
                raise Exception.Create('Tipo de campo não suportado!');
            end;
          except
            on e: Exception do
              raise Exception.Create('O valor informado (' + strValor + ') na propriedade ' + Prop.Name + ' no objeto ' + TObject(Obj).ClassName + 'não é compátivel com o tipo definido na classe (' + tipo_valor + ')!');
          end;
        end;           

        apelido_tab_estrangeira := PropriedadesCampo(Atributo).Apelido_Tabela_Estrangeira;

        //Verificando se o campo em questão faz referência a uma coluna de outra tabela
        if PropriedadesCampo(Atributo).Coluna_Estrangeira <> EmptyStr then begin
          tabela_estrangeira := PropriedadesCampo(Atributo).Tabela_Estrangeira;

          //Verificando se já foi adicionado essa tabela estrangeira no inner
          if not ApelidoTabelas.ContainsKey(apelido_tab_estrangeira) then begin
            ApelidoTabelas.Add(apelido_tab_estrangeira, x);

            //Montando a ligação, pois se entrou aqui, significa que ainda não existia o inner com essa tabela...
            scriptLigacoes.Add(IfThen(PropriedadesCampo(Atributo).Tipo_Ligacao = Inner, 'inner ', 'left ') +  ' join ' + tabela_estrangeira + ' ' + apelido_tab_estrangeira);
            scriptLigacoes.Add('on ' + apelido_tab_principal + '.' + PropriedadesCampo(Atributo).Name + ' = ' + apelido_tab_estrangeira +  '.' + PropriedadesCampo(Atributo).Coluna_Estrangeira);

            //Adicionando essa linha vazia, pois a mesma irá servir para quando for necessário adicionar mais um filtro no inner dessa tabela...
            scriptLigacoes.Add(EmptyStr);
          end
          else begin
            //Lembra que quando não existe a ligação com a tabela ainda, adicionamos uma linha em branco? Pois é, agora vamos adicionar o "AND" nela.
            i := scriptLigacoes.IndexOf(apelido_tab_estrangeira) + 2;

            //Porém, pode ser um inner com 3 colunas ou mais. Por isso vamos procurar a próxima linha em branco antes de adicionar
            while scriptLigacoes[i] <> EmptyStr do
              Inc(i);

            //Lembra que quando não existe a ligação com a tabela ainda, adicionamos uma linha em branco? Pois é, agora vamos adicionar o "AND" nela.
            scriptLigacoes.Insert(i, 'and ' + apelido_tab_principal + '.' + PropriedadesCampo(Atributo).Name + ' = ' + apelido_tab_estrangeira +  '.' + PropriedadesCampo(Atributo).Coluna_Estrangeira);
          end;

          strCampos := strCampos + '  ' + apelido_tab_estrangeira + '.' + PropriedadesCampo(Atributo).Name + ', ' + sLineBreak;
        end
        else
          strCampos := strCampos + '  ' + IfThen(apelido_tab_estrangeira = EmptyStr, apelido_tab_principal, apelido_tab_estrangeira) + '.' + PropriedadesCampo(Atributo).Name + ', ' + sLineBreak;


        if strValor <> EmptyStr then begin
          if adicionouWhere then
            strCondicaoWhere := strCondicaoWhere + 'and ' + IfThen(apelido_tab_estrangeira = EmptyStr, apelido_tab_principal, apelido_tab_estrangeira) + '.' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + sLineBreak
          else begin
            strCondicaoWhere := 'where ' + IfThen(apelido_tab_estrangeira = EmptyStr, apelido_tab_principal, apelido_tab_estrangeira) + '.' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + sLineBreak;
            adicionouWhere := True;
          end;
        end;
      end;
    end;
  end;

  strCampos := Trim(strCampos);
  strCampos := Copy(strCampos, 0, Length(strCampos) - 1);
  strCondicaoWhere := Trim(strCondicaoWhere);

  scriptSelect.Add('select');
  scriptSelect.Add('  ' + strCampos);
  scriptSelect.Add('from ');
  scriptSelect.Add('  ' + tabela_principal + ' ' + apelido_tab_principal);
  scriptSelect.Add(EmptyStr);

  if scriptLigacoes.GetText <> EmptyStr then
    scriptSelect.Add(scriptLigacoes.GetText);

  scriptSelect.Add(strCondicaoWhere);

  try
    //Executar SQL
    scriptSelect.SaveToFile('C:\Temp\select.sql');
    Result := scriptSelect.GetText;
  except
    on e: Exception do
    begin
      raise E.Create('Erro: ' + e.Message);
    end;
  end;

  scriptSelect.Free;
end;

class function TGenericDAO.Update<T>(Obj: T): Boolean;
var
  Contexto: TRttiContext;
  TypObj: TRttiType;
  Prop: TRttiProperty;
  Atributo: TCustomAttribute;

  scriptUpdate: TStringList;
  strValor: string;
  strCondicaoWhere: string;
  strCampos: string;
  adicionouWhere: Boolean;
begin
  scriptUpdate := TStringList.Create;
  strValor := EmptyStr;
  strCondicaoWhere := EmptyStr;
  strCampos := EmptyStr;
  adicionouWhere := False;

  scriptUpdate.Add('update ' + GetTableName(Obj) + ' set ');

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(TObject(Obj).ClassInfo);

  for Prop in TypObj.GetProperties do begin
    for Atributo in Prop.GetAttributes do begin
      if Atributo is PropriedadesCampo then begin
        strValor := EmptyStr;

        case Prop.GetValue(TObject(Obj)).Kind of
          tkWChar, tkLString, tkWString, tkString, tkChar, tkUString:
            strValor := QuotedStr(Prop.GetValue(TObject(Obj)).AsString);

          tkInteger, tkInt64:
            strValor := IntToStr(Prop.GetValue(TObject(Obj)).AsInteger);

          tkFloat:
            strValor := FloatToStr(Prop.GetValue(TObject(Obj)).AsExtended);
        else
          raise Exception.Create('Tipo de campo não suportado!');
        end;

        if PropriedadesCampo(Atributo).Chave then begin
//          if strValor = '' then
//            raise Exception.Create('Foi informado uma chave primaria sem valor!');

          if adicionouWhere then
            strCondicaoWhere := strCondicaoWhere + 'and ' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + sLineBreak
          else
            strCondicaoWhere := 'where ' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + sLineBreak;
        end
        else begin
          strCampos := strCampos + '  ' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + ',' + sLineBreak;
        end;
      end;
    end;
  end;
  strCampos := Trim(strCampos);
  strCampos := Copy(strCampos, 0, Length(strCampos) - 1);
  scriptUpdate.Add('  ' + strCampos);

  strCondicaoWhere := Trim(strCondicaoWhere);
  scriptUpdate.Add(strCondicaoWhere);
  try
    //Executar SQL
    scriptUpdate.SaveToFile('C:\Antonio\Update.sql');
    Result := True;
  except
    on e: Exception do
    begin
      raise E.Create('Erro: ' + e.Message);
    end;
  end;

  scriptUpdate.Free;

end;

end.
