unit uGenericDAO;

interface

uses
  RTTI, TypInfo, SysUtils, uAtribEntity, System.Generics.Collections, System.StrUtils, uTiposPrimitivos;

type
  TGenericDAO = class(TObject)
  private
    FClass: TObject;

    function GetNomeTabela: string;
  public
    constructor Create; virtual; abstract;

    function Select: string;
    function Insert: string;

    property Classe: TObject read FClass write FClass;

    class function Update(Obj: TObject): Boolean;

  end;

implementation

uses
  System.Classes;

const
  cIgualdade = ' = ';
  cIsNull    = ' is null ';

function TGenericDAO.GetNomeTabela: string;
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
      Exit(NomeTabela(Atributo).Nome_Tabela);
  end;
end;

function TGenericDAO.Insert: string;
var
  Contexto: TRttiContext;
  TypObj: TRttiType;
  Prop: TRttiProperty;
  comando_insert, campos, valores: String;
  tipo_valor: string;
  Atributo: TCustomAttribute;

  valor: string;
  value_str: TString;
  value_int: TInteger;
  value_double: TDouble;
  value_date: TDate;

begin
  comando_insert := EmptyStr;
  campos := EmptyStr;
  valores := EmptyStr;

  comando_insert := 'insert into ' + GetNomeTabela;

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(FClass.ClassInfo);

  for Prop in TypObj.GetProperties do begin
    try
      case Prop.GetValue(FClass).Kind of
        tkClass: Break;

        tkRecord: begin
          if Prop.GetValue(Fclass).IsType(TypeInfo(TString)) then begin
            tipo_valor := 'TString';

            value_str := Prop.GetValue(FClass).AsType<TString>;

            if value_str.HasValue then
              valor := QuotedStr(value_str) + ', '
            else
              valor := 'null' + ', ';
          end
          else if Prop.GetValue(Fclass).IsType(TypeInfo(TInteger)) then begin
            tipo_valor := 'TInteger';

            value_int := Prop.GetValue(FClass).AsType<TInteger>;

            if value_int.HasValue then
              valor := IntToStr(value_int) + ', '
            else
              valor := 'null' + ', ';
          end
          else if Prop.GetValue(Fclass).IsType(TypeInfo(TDouble)) then begin
            tipo_valor := 'TDouble';

            value_double := Prop.GetValue(FClass).AsType<TDouble>;

            if value_double.HasValue then
              valor := FloatToStr(value_double) + ', '
            else
              valor := 'null' + ', ';
          end
          else if Prop.GetValue(Fclass).IsType(TypeInfo(TDate)) then begin
            tipo_valor := 'TDate';

            value_date := Prop.GetValue(FClass).AsType<TDate>;

            if value_date.HasValue then
              valor := DateTimeToStr(value_date) + ', '
            else
              valor := 'null' + ', ';
          end
        end;
      end;
    except
      on e: Exception do
        raise Exception.Create('O valor informado (' + valores + ') na propriedade "' + Prop.Name + '" no objeto ' + FClass.ClassName + ' não é compátivel com o tipo definido na classe (' + tipo_valor + ')!');
    end;

    for Atributo in Prop.GetAttributes do begin
      if Atributo is DadosColuna then begin
        if DadosColuna(Atributo).Somente_Leitura then
          Break;

        campos := campos + DadosColuna(Atributo).Nome_Coluna + ', ';
        valores := valores + valor;
        Break;
      end;

      if Atributo is ChaveEstrangeira then
        Continue;
    end;
  end;

  campos := Copy(campos, 1, Length(campos) - 2);
  valores := Copy(valores, 1, Length(valores) - 2);
  comando_insert := comando_insert + ' ( ' + sLineBreak + campos + sLineBreak + ' )  values ( ' + sLineBreak + valores + sLineBreak + ' )';

  try
    //Executar SQL
    Result := comando_insert;
  except
    on e: Exception do
    begin
      raise E.Create('Erro: ' + e.Message);
    end;
  end;
end;

function TGenericDAO.Select: string;
var
  contexto: TRttiContext;
  type_obj: TRttiType;
  prop: TRttiProperty;
  atributo: TCustomAttribute;

  script_select: TStringList;
  script_ligacoes: TStringList;
  str_valor: string;
  str_condicaoWhere: string;
  str_campos: string;
  adicionou_where: Boolean;

  apelido_tabelas: TDictionary<string, string>;
  apelido_tab_estrangeira: string;
  tabela_estrangeira: string;
  tabela_principal: string;
  apelido_tab_principal: string;
  str: string;
  tipo_valor: string;
  coluna: string;

  i: Integer;

  value_str: TString;
  value_int: TInteger;
  value_double: TDouble;
  value_date: TDate;

  filtrar_campo: Boolean;

  function Clausula: string;
  begin
    if adicionou_where then
      Result := ' and '
    else begin
      adicionou_where := True;
      Result := ' where ';
    end;
  end;

  function strIgualdade: string;
  begin
    Result := IfThen(str_valor <> ' is null ', ' = ');
  end;

begin
  script_ligacoes := TStringList.Create;
  str_valor := EmptyStr;
  str_condicaoWhere := EmptyStr;
  str_campos := EmptyStr;
  adicionou_where := False;
  str := EmptyStr;

  tabela_principal := GetNomeTabela;
  apelido_tab_principal := Copy(tabela_principal, 1, 3);
  apelido_tabelas := TDictionary<string, string>.Create;
  apelido_tabelas.Add(tabela_principal, apelido_tab_principal);

  contexto := TRttiContext.Create;
  type_obj := contexto.GetType(FClass.ClassInfo);

  //Buscando as propertys do objeto
  for prop in type_obj.GetProperties do begin
    filtrar_campo := True;

    //Verificando se existe valor
    if not prop.GetValue(FClass).IsEmpty then
    begin
      str_valor := EmptyStr;

      try
        case prop.GetValue(FClass).Kind of
          //Tive que fazer isso porque o RTTI percorre também as propertys da classe pai
          tkClass: Break;

          tkRecord: begin
            if prop.GetValue(Fclass).IsType(TypeInfo(TString)) then begin
              tipo_valor := 'TString';

              value_str := prop.GetValue(FClass).AsType<TString>;

              if value_str.HasValue then
                str_valor := QuotedStr(value_str)
              else if value_str.Value_Null then
                str_valor := ' is null '
              else
               filtrar_campo := False;
            end
            else if prop.GetValue(Fclass).IsType(TypeInfo(TInteger)) then begin
              tipo_valor := 'TInteger';

              value_int := prop.GetValue(FClass).AsType<TInteger>;

              if value_int.HasValue then
                str_valor := IntToStr(value_int)
              else if value_int.Value_Null then
                str_valor := ' is null '
              else
                filtrar_campo := False;
            end
            else if prop.GetValue(Fclass).IsType(TypeInfo(TDouble)) then begin
              tipo_valor := 'TDouble';

              value_double := prop.GetValue(FClass).AsType<TDouble>;

              if value_double.HasValue then
                str_valor := FloatToStr(value_double)
              else if value_double.Value_Null then
                str_valor := ' is null '
              else
                filtrar_campo := False;
            end
            else if prop.GetValue(Fclass).IsType(TypeInfo(TDate)) then begin
              tipo_valor := 'TDate';

              value_date := prop.GetValue(FClass).AsType<TDate>;

              if value_date.HasValue then
                str_valor := DateTimeToStr(value_date)
              else if value_date.Value_Null then
                str_valor := ' is null '
              else
                filtrar_campo := False;
            end
          end;
        end;
      except
        on e: Exception do
          raise Exception.Create('O valor informado (' + str_valor + ') na propriedade "' + prop.Name + '" no objeto ' + FClass.ClassName + ' não é compátivel com o tipo definido na classe (' + tipo_valor + ')!');
      end;
    end;

    apelido_tab_estrangeira := EmptyStr;
    coluna := EmptyStr;

    for atributo in prop.GetAttributes do begin
      if atributo is DadosColuna then begin
        if coluna = EmptyStr then
          coluna := DadosColuna(atributo).Nome_Coluna;

        Continue;
      end;

      if atributo is ChaveEstrangeira then begin
        apelido_tab_estrangeira := ChaveEstrangeira(atributo).Apelido_Tabela_Estrangeira;

        if apelido_tab_estrangeira = EmptyStr then
          Continue;

        //Verificando se o campo em questão faz referência a uma coluna de outra tabela
        if ChaveEstrangeira(atributo).Coluna_Estrangeira <> EmptyStr then begin
          tabela_estrangeira := ChaveEstrangeira(atributo).Tabela_Estrangeira;

          //Verificando se já foi adicionado essa tabela estrangeira no inner
          if not apelido_tabelas.ContainsKey(apelido_tab_estrangeira) then begin
            apelido_tabelas.Add(apelido_tab_estrangeira, str);

            //Montando a ligação, pois se entrou aqui, significa que ainda não existia o inner com essa tabela...
            script_ligacoes.Add(IfThen(ChaveEstrangeira(atributo).Tipo_Ligacao = Inner, 'inner ', 'left ') +  ' join ' + tabela_estrangeira + ' ' + apelido_tab_estrangeira);
            script_ligacoes.Add('on ' + apelido_tab_principal + '.' + ChaveEstrangeira(atributo).Coluna_Estrangeira + ' = ' + apelido_tab_estrangeira +  '.' + ChaveEstrangeira(atributo).Coluna_Estrangeira);

            //Adicionando essa linha vazia, pois a mesma irá servir para quando for necessário adicionar mais um filtro no inner dessa tabela...
            script_ligacoes.Add(EmptyStr);
          end
          else begin
            //Lembra que quando não existe a ligação com a tabela ainda, adicionamos uma linha em branco? Pois é, agora vamos adicionar o "AND" nela.
            i := script_ligacoes.IndexOf(apelido_tab_estrangeira) + 2;

            //Porém, pode ser um inner com 3 colunas ou mais. Por isso vamos procurar a próxima linha em branco antes de adicionar
            while script_ligacoes[i] <> EmptyStr do
              Inc(i);

            //Lembra que quando não existe a ligação com a tabela ainda, adicionamos uma linha em branco? Pois é, agora vamos adicionar o "AND" nela.
            script_ligacoes.Insert(i, 'and ' + apelido_tab_principal + '.' + ChaveEstrangeira(atributo).Coluna_Estrangeira + ' = ' + apelido_tab_estrangeira +  '.' + ChaveEstrangeira(atributo).Coluna_Estrangeira);
          end;

          coluna := ChaveEstrangeira(atributo).Coluna_Estrangeira;
        end;
      end;

    end; //For atributo

    str_campos := str_campos + '  ' + IfThen(apelido_tab_estrangeira <> EmptyStr, apelido_tab_estrangeira, apelido_tab_principal) + '.' + coluna + ', ' + sLineBreak;

    //Essa variável é marcada com True se existir algum valor na propriedade percorrida.
    if filtrar_campo then
      str_condicaoWhere := str_condicaoWhere + Clausula + IfThen(apelido_tab_estrangeira = EmptyStr, apelido_tab_principal, apelido_tab_estrangeira) + '.' + coluna + strIgualdade + str_valor + sLineBreak;
  end;

  str_campos := Trim(str_campos);
  str_campos := Copy(str_campos, 0, Length(str_campos) - 1);
  str_condicaoWhere := Trim(str_condicaoWhere);

  script_select := TStringList.Create;
  script_select.Add('select');
  script_select.Add('  ' + str_campos);
  script_select.Add('from ');
  script_select.Add('  ' + tabela_principal + ' ' + apelido_tab_principal);
  script_select.Add(EmptyStr);

  if script_ligacoes.GetText <> EmptyStr then
    script_select.Add(script_ligacoes.GetText);

  script_select.Add(str_condicaoWhere);

  try
    //Executar SQL
    Result := script_select.GetText;
  except
    on e: Exception do
    begin
      raise E.Create('Erro: ' + e.Message);
    end;
  end;

  script_select.Free;
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
begin
  scriptUpdate := TStringList.Create;
  strValor := EmptyStr;
  strCondicaoWhere := EmptyStr;
  strCampos := EmptyStr;

//  scriptUpdate.Add('update ' + GetNomeTabela(Obj) + ' set ');

  Contexto := TRttiContext.Create;
  TypObj := Contexto.GetType(Obj.ClassInfo);

  for Prop in TypObj.GetProperties do begin
    for Atributo in Prop.GetAttributes do begin
//      if Atributo is PropriedadesCampo then begin
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

//        if PropriedadesCampo(Atributo).Chave then begin
//          if adicionouWhere then
//            strCondicaoWhere := strCondicaoWhere + 'and ' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + sLineBreak
//          else
//            strCondicaoWhere := 'where ' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + sLineBreak;
//        end
//        else begin
//          strCampos := strCampos + '  ' + PropriedadesCampo(Atributo).Name + ' = ' + strValor + ',' + sLineBreak;
//        end;
//      end;
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
