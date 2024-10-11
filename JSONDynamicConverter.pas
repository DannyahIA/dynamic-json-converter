unit JSONDynamicConverter;

interface

uses
  DateUtils,
  System.Rtti,
  System.JSON,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections;

type
  TJSONDynConverter = class
    { ---------------------- JSON TO OBJECT  ---------------------- }

    // JSONString to Object
    procedure JSONStringToObject(AJSONString: string; AObject: TObject);

    // JSONObject to Object
    procedure JSONToObject(AJSONObject: TJSONObject; AObject: TObject);

    // JSONString to ObjectList
    procedure JSONStringToObjectList<T: class, constructor>(AJSONString: string;
      AObjectList: TObjectList<T>);

    // JSONArray to ObjectList
    procedure JSONArrayToObjectList<T: class, constructor>
      (AJSONArray: TJSONArray; AObjectList: TObjectList<T>);

    { ---------------------- OBJECT TO JSON ---------------------- }

    // Object to JSONObject
    procedure ObjectToJSONObject(AObject: TObject;
      var AJSONObject: TJSONObject);

    // ObjectList to JSONArray
    procedure ObjectListToJSONArray<T: class>(AObjectList: TObjectList<T>;
      var AJSONArray: TJSONArray);

  private
    { --------------------------- UTILS --------------------------- }

    // Check Json Value Type
    procedure CheckJSONValueType(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject);
  end;

implementation

{ --------------------------- JSON TO OBJECT --------------------------- }

{ JSONString to Object }
procedure TJSONDynConverter.JSONStringToObject(AJSONString: string;
  AObject: TObject);
begin
  var
  AJSONObject := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
  try
    JSONToObject(AJSONObject, AObject);
  finally
    AJSONObject.Free;
  end;
end;

{ JSONObject to Object }
procedure TJSONDynConverter.JSONToObject(AJSONObject: TJSONObject;
  AObject: TObject);
var
  Context: TRttiContext; // Contexto RTTI para acessar as propriedades do objeto
  RttiType: TRttiType; // Representa o tipo do objeto usando RTTI
  Prop: TRttiProperty; // Propriedade RTTI para cada campo no objeto
  JsonPair: TJSONPair; // Representa um par de chave-valor no JSON
  JSONArray: TJSONArray; // Representa um array no JSON
  ElemClass: TClass; // Classe dos elementos no array din�mico
  I: integer; // Iterador de loop
  SubObject: TObject; // Objeto auxiliar para elementos de arrays
  ArrayValue: TValue; // Valor do array final para ser atribu�do � propriedade
  TValueArray: TArray<TValue>;
  // Array de valores para os elementos do array din�mico
begin
  Context := TRttiContext.Create; // Inicializa o contexto RTTI
  try
    RttiType := Context.GetType(AObject.ClassType);
    // Obt�m o tipo RTTI do objeto

    // Itera sobre cada par chave-valor do JSON
    for JsonPair in AJSONObject do
    begin
      // Tenta obter a propriedade do objeto correspondente � chave do JSON
      Prop := RttiType.GetProperty(JsonPair.JSONString.Value);

      // Verifica se a propriedade existe no objeto
      if Assigned(Prop) then
      begin
        // Verifica se a propriedade � um array din�mico e se o valor no JSON � um array
        if (Prop.PropertyType.TypeKind = tkDynArray) and
          (JsonPair.JsonValue is TJSONArray) then
        begin
          JSONArray := TJSONArray(JsonPair.JsonValue);
          // Converte o valor JSON para TJSONArray
          SetLength(TValueArray, JSONArray.Count);
          // Define o tamanho do array com base na quantidade de itens do JSON

          // Obt�m a classe dos elementos do array din�mico
          ElemClass := (Prop.PropertyType as TRttiDynamicArrayType)
            .ElementType.AsInstance.MetaclassType;

          // Itera sobre os itens do array no JSON
          for I := 0 to JSONArray.Count - 1 do
          begin
            // Se o item do array no JSON for um objeto, processa recursivamente
            if JSONArray.Items[I] is TJSONObject then
            begin
              SubObject := ElemClass.Create;
              // Cria uma inst�ncia do tipo de elemento do array
              try
                // Chama recursivamente para preencher o subobjeto com o JSON
                JSONToObject(JSONArray.Items[I] as TJSONObject, SubObject);
                TValueArray[I] := TValue.From(SubObject);
                // Armazena o subobjeto no array
              except
                SubObject.Free; // Libera o subobjeto em caso de erro
                raise;
              end;
            end
            else
              raise Exception.CreateFmt
                ('Array item at index %d is not a valid JSON object.', [I]);
            // Erro se o item do array n�o for um objeto JSON
          end;

          // Converte o array de valores para um TValue e o atribui � propriedade do objeto
          ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
          Prop.SetValue(AObject, ArrayValue);
        end
        else
        begin
          CheckJSONValueType(JsonPair, Prop, AObject);
          // Lida com propriedades de tipos simples (strings, n�meros, etc.)
        end;
      end;
    end;
  finally
    Context.Free; // Libera o contexto RTTI ao final
  end;
end;

{ JSONString to ObjectList }
procedure TJSONDynConverter.JSONStringToObjectList<T>(AJSONString: string;
  AObjectList: TObjectList<T>);
var
  I: integer;
  SubObject: T;
begin
  var
  AJSONArray := TJSONObject.ParseJSONValue(AJSONString) as TJSONArray;
  try
    JSONArrayToObjectList<T>(AJSONArray, AObjectList);
  finally
    AJSONArray.Free;
  end;
end;

{ JSONArray to ObjectList }
procedure TJSONDynConverter.JSONArrayToObjectList<T>(AJSONArray: TJSONArray;
  AObjectList: TObjectList<T>);
var
  I: integer;
  SubObject: T;
begin
  // Limpa a lista para adicionar novos objetos
  AObjectList.Clear;

  // Itera pelos itens do JSON array
  for I := 0 to AJSONArray.Count - 1 do
  begin
    if AJSONArray.Items[I] is TJSONObject then
    begin
      SubObject := T.Create; // Cria o objeto do tipo T (classe gen�rica)
      try
        JSONToObject(AJSONArray.Items[I] as TJSONObject, SubObject);
        // Converte o JSON para o objeto
        AObjectList.Add(SubObject); // Adiciona o objeto � lista
      except
        SubObject.Free; // Libera o objeto em caso de erro
        raise;
      end;
    end
    else
      raise Exception.CreateFmt
        ('Item no �ndice %d n�o � um objeto JSON v�lido.', [I]);
  end;
end;

{ --------------------------- OBJECT TO JSON --------------------------- }

{ Object to JSONObject }
procedure TJSONDynConverter.ObjectToJSONObject(AObject: TObject;
  var AJSONObject: TJSONObject);
var
  Context: TRttiContext; // Contexto para RTTI
  RttiType: TRttiType; // Tipo RTTI do objeto
  Prop: TRttiProperty; // Propriedade atual do objeto
  PropValue: TValue; // Valor da propriedade atual
  SubJSON: TJSONObject; // Objeto JSON para propriedades que s�o objetos
  JSONArray: TJSONArray;
  // Array JSON para propriedades que s�o arrays din�micos
  I: integer; // Contador de loop
  DynArrayValue: TValue; // Valor de cada elemento do array din�mico
begin
  // Cria o contexto RTTI para acessar informa��es sobre o tipo do objeto
  Context := TRttiContext.Create;
  try
    // Obt�m o tipo RTTI do objeto passado como par�metro
    RttiType := Context.GetType(AObject.ClassType);

    // Itera pelas propriedades do objeto utilizando RTTI
    for Prop in RttiType.GetProperties do
    begin
      // Verifica se a propriedade � leg�vel (possui m�todo getter)
      if Prop.IsReadable then
      begin
        // Obt�m o valor atual da propriedade
        PropValue := Prop.GetValue(AObject);

        // Lida com propriedades que s�o arrays din�micos
        if (Prop.PropertyType.TypeKind = tkDynArray) then
        begin
          // Cria um novo array JSON para armazenar os elementos do array din�mico
          JSONArray := TJSONArray.Create;

          // Itera pelos elementos do array din�mico
          for I := 0 to PropValue.GetArrayLength - 1 do
          begin
            // Obt�m o valor de cada elemento do array
            DynArrayValue := PropValue.GetArrayElement(I);
            // Verifica se o elemento � um objeto
            if DynArrayValue.IsObject then
            begin
              // Cria um novo objeto JSON para o elemento
              SubJSON := TJSONObject.Create;
              // Chama recursivamente a fun��o para converter o objeto em JSON
              ObjectToJSONObject(DynArrayValue.AsObject, SubJSON);
              // Adiciona o objeto JSON ao array JSON
              JSONArray.AddElement(SubJSON);
            end
            // Verifica se o elemento � um n�mero inteiro
            else if DynArrayValue.Kind = tkInteger then
              JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsInteger))
              // Verifica se o elemento � um n�mero em ponto flutuante
            else if DynArrayValue.Kind = tkFloat then
              JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsExtended))
              // Verifica se o elemento � uma string
            else if DynArrayValue.Kind = tkString then
              JSONArray.AddElement(TJSONString.Create(DynArrayValue.AsString));
          end;

          // Adiciona o array JSON � propriedade correspondente no objeto JSON final
          AJSONObject.AddPair(Prop.Name, JSONArray);
        end
        // Lida com propriedades que s�o objetos aninhados
        else if PropValue.IsObject then
        begin
          // Cria um novo objeto JSON para a propriedade aninhada
          SubJSON := TJSONObject.Create;
          // Chama recursivamente a fun��o para converter o objeto em JSON
          ObjectToJSONObject(PropValue.AsObject, SubJSON);
          // Adiciona o objeto JSON aninhado ao objeto JSON final
          AJSONObject.AddPair(Prop.Name, SubJSON);
        end
        // Lida com propriedades que s�o valores simples
        else if PropValue.Kind = tkInteger then
          AJSONObject.AddPair(Prop.Name,
            TJSONNumber.Create(PropValue.AsInteger))
        else if PropValue.Kind = tkFloat then
          AJSONObject.AddPair(Prop.Name,
            TJSONNumber.Create(PropValue.AsExtended))
        else if PropValue.Kind = tkUString then
          AJSONObject.AddPair(Prop.Name, TJSONString.Create(PropValue.AsString))
        else if PropValue.Kind = tkEnumeration then
        begin
          // Lida com valores booleanos
          if PropValue.TypeInfo = TypeInfo(Boolean) then
            AJSONObject.AddPair(Prop.Name,
              TJSONBool.Create(PropValue.AsBoolean))
          else
            AJSONObject.AddPair(Prop.Name,
              TJSONString.Create(PropValue.ToString));
        end
        // Lida com registros (como TDate e TDateTime)
        else if PropValue.Kind = tkRecord then
        begin
          // Verifica se a propriedade � do tipo TDate e a converte
          if Prop.PropertyType.Handle = TypeInfo(TDate) then
            AJSONObject.AddPair(Prop.Name,
              TJSONString.Create(DateToISO8601(PropValue.AsType<TDate>, False)))
            // Verifica se a propriedade � do tipo TDateTime e a converte
          else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
            AJSONObject.AddPair(Prop.Name,
              TJSONString.Create
              (DateToISO8601(PropValue.AsType<TDateTime>, True)));
        end;
      end;
    end;
  finally
    // Libera o contexto RTTI
    Context.Free;
  end;
end;

{ ObjectList to JSONArray }
procedure TJSONDynConverter.ObjectListToJSONArray<T>
  (AObjectList: TObjectList<T>; var AJSONArray: TJSONArray);
var
  I: integer;
  SubJSON: TJSONObject;
begin
  AJSONArray := TJSONArray.Create; // Cria o array JSON

  // Itera pela lista de objetos
  for I := 0 to AObjectList.Count - 1 do
  begin
    SubJSON := TJSONObject.Create; // Cria um novo objeto JSON para cada item
    ObjectToJSONObject(AObjectList[I], SubJSON); // Converte o objeto para JSON
    AJSONArray.AddElement(SubJSON); // Adiciona o objeto JSON ao array
  end;
end;

{ --------------------------- UTILS --------------------------- }

{ CheckValueType }
procedure TJSONDynConverter.CheckJSONValueType(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
begin
  // Verifica se o valor do campo no JSON � null
  if JsonPair.JsonValue.Null then
    Prop.SetValue(AObject, TValue.Empty) // Atribui um valor vazio
  else if JsonPair.JsonValue is TJSONNumber then
  begin
    // Verifica se a propriedade � do tipo inteiro
    if Prop.PropertyType.Handle = TypeInfo(integer) then
      Prop.SetValue(AObject,
        TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
      // Verifica se a propriedade � do tipo double (decimal)
    else if Prop.PropertyType.Handle = TypeInfo(double) then
      Prop.SetValue(AObject, TValue.From<double>(TJSONNumber(JsonPair.JsonValue)
        .AsDouble))
    else
      raise Exception.CreateFmt
        ('Tipo num�rico n�o suportado para a propriedade "%s".', [Prop.Name]);
  end
  else if JsonPair.JsonValue is TJSONTrue then
    Prop.SetValue(AObject, TValue.From<Boolean>(True))
    // Atribui valor booleano True
  else if JsonPair.JsonValue is TJSONFalse then
    Prop.SetValue(AObject, TValue.From<Boolean>(False))
    // Atribui valor booleano False
  else if JsonPair.JsonValue is TJSONString then
  begin
    // Verifica se a propriedade � uma data
    if Prop.PropertyType.Handle = TypeInfo(TDate) then
      Prop.SetValue(AObject,
        TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue)
        .Value, False)))
      // Verifica se a propriedade � um DateTime
    else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
      Prop.SetValue(AObject,
        TValue.From<TDateTime>(ISO8601ToDate(TJSONString(JsonPair.JsonValue)
        .Value, True)))
    else
      // Atribui o valor como string
      Prop.SetValue(AObject,
        TValue.From<string>(TJSONString(JsonPair.JsonValue).Value));
  end
  else
    raise Exception.CreateFmt
      ('Tipo de JSON n�o suportado para a propriedade "%s".', [Prop.Name]);
  // Erro se o tipo JSON n�o for suportado
end;

end.
