unit uGenericDAO;

interface

uses
  RTTI, TypInfo, SysUtils, uAtribEntity, System.Generics.Collections, System.StrUtils, uTiposPrimitivos;

type
  TGenericDAO = class(TObject)
  private
    FClass: TObject;

    function GetTableName: string;

    class function GetFields(Atributos: TArray<TCustomAttribute>): string;
  public
    constructor Create; virtual; abstract;

    function SelectBasico: string;

    property Classe: TObject read FClass write FClass;

    class function Insert(Obj: TObject): Boolean;
    class function Update(Obj: TObject): Boolean;

    class function SelectAll(Obj: TObject): string;

  end;

implementation

uses
  System.Classes;

const
  cIgualdade = ' = ';
  cIsNull    = ' is null ';

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

function TGenericDAO.GetTableName: string;
var
  Contexto: TRttiContext;
  TypObj: TRttiType;
  Atributo: TCustomAttribute;

begin
  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(FClass.ClassInfo);
  for Atributo in TypObj.GetAttributes do
  begin
    if Atributo is NomeTabela then
      Exit(TableName(Atributo).Name);
  end;
end;

class function TGenericDAO.Insert(Obj: TObject): Boolean;
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

//  strInsert := 'insert into ' + GetTableName;

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(Obj.ClassInfo);

  for Prop in TypObj.GetProperties do begin
    for Atributo in Prop.GetAttributes do begin
        if Atributo is FieldName then begin
           strFields := strFields + FieldName(Atributo).Name  + ', ';
           case Prop.GetValue(Obj).Kind of

             tkWChar, tkLString, tkWString, tkString, tkChar, tkUString:
               strValues := strValues + QuotedStr(Prop.GetValue(Obj).AsString) + ', ';

             tkInteger, tkInt64:
               strValues := strValues + IntToStr(Prop.GetValue(Obj).AsInteger) + ', ';

             tkFloat:
               strValues := strValues + FloatToStr(Prop.GetValue(Obj).AsExtended) + ', ';

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

class function TGenericDAO.SelectAll(Obj: TObject): string;
begin
//  Result := 'SELECT T1.* from ' + GetTableName(Obj) + 'T1';
end;

function TGenericDAO.SelectBasico: string;
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

  ApelidoTabelas: TDictionary<string, string>;
  apelido_tab_estrangeira: string;
  tabela_estrangeira: string;
  tabela_principal: string;
  apelido_tab_principal: string;
  x: string;
  tipo_valor: string;

  i: Integer;

  value_str: TString;
  value_int: TInteger;
  value_double: TDouble;
  value_date: TDate;

  filtrar_campo: Boolean;

  function Clausula: string;
  begin
    if adicionouWhere then
      Result := ' and '
    else begin
      adicionouWhere := True;
      Result := ' where ';
    end;
  end;

  function strIgualdade: string;
  begin
    Result := IfThen(strValor <> ' is null ', ' = ');
  end;

begin
  scriptLigacoes := TStringList.Create;
  strValor := EmptyStr;
  strCondicaoWhere := EmptyStr;
  strCampos := EmptyStr;
  adicionouWhere := False;

  tabela_principal := GetTableName;
  apelido_tab_principal := Copy(tabela_principal, 1, 3);
  ApelidoTabelas := TDictionary<string, string>.Create;
  ApelidoTabelas.Add(tabela_principal, apelido_tab_principal);

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(FClass.ClassInfo);

  //Buscando as propertys do objeto
  for Prop in TypObj.GetProperties do begin
//    Value := nil;
    filtrar_campo := True;

    //Verificando se existe valor
    if not Prop.GetValue(FClass).IsEmpty then
    begin
      strValor := EmptyStr;

      try
        case Prop.GetValue(FClass).Kind of
          //Tive que fazer isso porque o RTTI percorre também as propertys da classe pai
          tkClass: Break;

          tkRecord: begin
            if Prop.GetValue(Fclass).IsType(TypeInfo(TString)) then begin
              tipo_valor := 'TString';

              value_str := Prop.GetValue(FClass).AsType<TString>;

              if value_str.HasValue then
                strValor := QuotedStr(value_str)
              else if value_str.FiltrarNull then
                strValor := ' is null '
              else
               filtrar_campo := False;
            end
            else if Prop.GetValue(Fclass).IsType(TypeInfo(TInteger)) then begin
              tipo_valor := 'TInteger';

              value_int := Prop.GetValue(FClass).AsType<TInteger>;

              if value_int.HasValue then
                strValor := IntToStr(value_int)
              else if value_int.FiltrarNull then
                strValor := ' is null '
              else
                filtrar_campo := False;
            end
            else if Prop.GetValue(Fclass).IsType(TypeInfo(TDouble)) then begin
              tipo_valor := 'TDouble';

              value_double := Prop.GetValue(FClass).AsType<TDouble>;

              if value_double.HasValue then
                strValor := FloatToStr(value_double)
              else if value_double.FiltrarNull then
                strValor := ' is null '
              else
                filtrar_campo := False;
            end
            else if Prop.GetValue(Fclass).IsType(TypeInfo(TDate)) then begin
              tipo_valor := 'TDate';

              value_date := Prop.GetValue(FClass).AsType<TDate>;

              if value_date.HasValue then
                strValor := DateTimeToStr(value_date)
              else if value_date.FiltrarNull then
                strValor := ' is null '
              else
                filtrar_campo := False;
            end
          end;
          else
            raise Exception.Create('Tipo de campo não suportado!');
        end;
      except
        on e: Exception do
          raise Exception.Create('O valor informado (' + strValor + ') na propriedade "' + Prop.Name + '" no objeto ' + FClass.ClassName + ' não é compátivel com o tipo definido na classe (' + tipo_valor + ')!');
      end;
    end;

    apelido_tab_estrangeira := EmptyStr;

    for Atributo in Prop.GetAttributes do begin
      if Atributo is NomeCampo then
        strCampos := strCampos + '  ' + apelido_tab_principal + '.' + NomeCampo(Atributo).Nome_Coluna + ', ' + sLineBreak
      else if Atributo is ChaveEstrangeira then begin
        apelido_tab_estrangeira := ChaveEstrangeira(Atributo).Apelido_Tabela_Estrangeira;

        //Verificando se o campo em questão faz referência a uma coluna de outra tabela
        if ChaveEstrangeira(Atributo).Coluna_Estrangeira <> EmptyStr then begin
          tabela_estrangeira := ChaveEstrangeira(Atributo).Tabela_Estrangeira;

          //Verificando se já foi adicionado essa tabela estrangeira no inner
          if not ApelidoTabelas.ContainsKey(apelido_tab_estrangeira) then begin
            ApelidoTabelas.Add(apelido_tab_estrangeira, x);

            //Montando a ligação, pois se entrou aqui, significa que ainda não existia o inner com essa tabela...
            scriptLigacoes.Add(IfThen(ChaveEstrangeira(Atributo).Tipo_Ligacao = Inner, 'inner ', 'left ') +  ' join ' + tabela_estrangeira + ' ' + apelido_tab_estrangeira);
            scriptLigacoes.Add('on ' + apelido_tab_principal + '.' + ChaveEstrangeira(Atributo).Coluna_Estrangeira + ' = ' + apelido_tab_estrangeira +  '.' + ChaveEstrangeira(Atributo).Coluna_Estrangeira);

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
            scriptLigacoes.Insert(i, 'and ' + apelido_tab_principal + '.' + ChaveEstrangeira(Atributo).Coluna_Estrangeira + ' = ' + apelido_tab_estrangeira +  '.' + ChaveEstrangeira(Atributo).Coluna_Estrangeira);
          end;

          strCampos := strCampos + '  ' + apelido_tab_estrangeira + '.' + ChaveEstrangeira(Atributo).Coluna_Estrangeira + ', ' + sLineBreak;
        end;
      end;
    end; //for Atributo

    if filtrar_campo then
      strCondicaoWhere := strCondicaoWhere + Clausula + IfThen(apelido_tab_estrangeira = EmptyStr, apelido_tab_principal, apelido_tab_estrangeira) + '.' + PropriedadesCampo(Atributo).Name + strIgualdade + strValor + sLineBreak;
  end;

  strCampos := Trim(strCampos);
  strCampos := Copy(strCampos, 0, Length(strCampos) - 1);
  strCondicaoWhere := Trim(strCondicaoWhere);

  scriptSelect := TStringList.Create;
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

class function TGenericDAO.Update(Obj: TObject): Boolean;
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

//  scriptUpdate.Add('update ' + GetTableName(Obj) + ' set ');

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(Obj.ClassInfo);

  for Prop in TypObj.GetProperties do begin
    for Atributo in Prop.GetAttributes do begin
      if Atributo is PropriedadesCampo then begin
        strValor := EmptyStr;

        case Prop.GetValue(Obj).Kind of
          tkWChar, tkLString, tkWString, tkString, tkChar, tkUString:
            strValor := QuotedStr(Prop.GetValue(Obj).AsString);

          tkInteger, tkInt64:
            strValor := IntToStr(Prop.GetValue(Obj).AsInteger);

          tkFloat:
            strValor := FloatToStr(Prop.GetValue(Obj).AsExtended);
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
