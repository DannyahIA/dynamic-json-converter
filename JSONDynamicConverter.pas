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
  TJsonDynamicConverter = class
    procedure JSONToObject(AObject: TObject; AJSON: TJSONObject);
    procedure ObjectToJSON(AObject: TObject; var AJSON: TJSONObject);
    procedure JSONArrayToObjectList<T: class, constructor>(AArray: TJSONArray;
      AObjectList: TObjectList<T>);

    procedure ObjectListToJSONArray<T: class>(AObjectList: TObjectList<T>;
      var AJSONArray: TJSONArray);
  private
    procedure CheckValueType(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject);
  end;

implementation

{ --------------------------- TJsonDynamicConverter --------------------------- }

{ JSONObject to Object }
procedure TJsonDynamicConverter.JSONToObject(AObject: TObject;
  AJSON: TJSONObject);
var
  Context: TRttiContext; // Contexto RTTI para acessar as propriedades do objeto
  RttiType: TRttiType; // Representa o tipo do objeto usando RTTI
  Prop: TRttiProperty; // Propriedade RTTI para cada campo no objeto
  JsonPair: TJSONPair; // Representa um par de chave-valor no JSON
  JSONArray: TJSONArray; // Representa um array no JSON
  ElemClass: TClass; // Classe dos elementos no array dinâmico
  I: integer; // Iterador de loop
  SubObject: TObject; // Objeto auxiliar para elementos de arrays
  ArrayValue: TValue; // Valor do array final para ser atribuído à propriedade
  TValueArray: TArray<TValue>;
  // Array de valores para os elementos do array dinâmico
begin
  Context := TRttiContext.Create; // Inicializa o contexto RTTI
  try
    RttiType := Context.GetType(AObject.ClassType);
    // Obtém o tipo RTTI do objeto

    // Itera sobre cada par chave-valor do JSON
    for JsonPair in AJSON do
    begin
      // Tenta obter a propriedade do objeto correspondente à chave do JSON
      Prop := RttiType.GetProperty(JsonPair.JsonString.Value);

      // Verifica se a propriedade existe no objeto
      if Assigned(Prop) then
      begin
        // Verifica se a propriedade é um array dinâmico e se o valor no JSON é um array
        if (Prop.PropertyType.TypeKind = tkDynArray) and
          (JsonPair.JsonValue is TJSONArray) then
        begin
          JSONArray := TJSONArray(JsonPair.JsonValue);
          // Converte o valor JSON para TJSONArray
          SetLength(TValueArray, JSONArray.Count);
          // Define o tamanho do array com base na quantidade de itens do JSON

          // Obtém a classe dos elementos do array dinâmico
          ElemClass := (Prop.PropertyType as TRttiDynamicArrayType)
            .ElementType.AsInstance.MetaclassType;

          // Itera sobre os itens do array no JSON
          for I := 0 to JSONArray.Count - 1 do
          begin
            // Se o item do array no JSON for um objeto, processa recursivamente
            if JSONArray.Items[I] is TJSONObject then
            begin
              SubObject := ElemClass.Create;
              // Cria uma instância do tipo de elemento do array
              try
                // Chama recursivamente para preencher o subobjeto com o JSON
                JSONToObject(SubObject, JSONArray.Items[I] as TJSONObject);
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
            // Erro se o item do array não for um objeto JSON
          end;

          // Converte o array de valores para um TValue e o atribui à propriedade do objeto
          ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
          Prop.SetValue(AObject, ArrayValue);
        end
        else
        begin
          CheckValueType(JsonPair, Prop, AObject);
          // Lida com propriedades de tipos simples (strings, números, etc.)
        end;
      end;
    end;
  finally
    Context.Free; // Libera o contexto RTTI ao final
  end;
end;

{ JSONArray to ObjectList }
procedure TJsonDynamicConverter.JSONArrayToObjectList<T>(AArray: TJSONArray;
  AObjectList: TObjectList<T>);
var
  I: integer;
  SubObject: T;
begin
  // Limpa a lista para adicionar novos objetos
  AObjectList.Clear;

  // Itera pelos itens do JSON array
  for I := 0 to AArray.Count - 1 do
  begin
    if AArray.Items[I] is TJSONObject then
    begin
      SubObject := T.Create; // Cria o objeto do tipo T (classe genérica)
      try
        JSONToObject(SubObject, AArray.Items[I] as TJSONObject);
        // Converte o JSON para o objeto
        AObjectList.Add(SubObject); // Adiciona o objeto à lista
      except
        SubObject.Free; // Libera o objeto em caso de erro
        raise;
      end;
    end
    else
      raise Exception.CreateFmt
        ('Item no índice %d não é um objeto JSON válido.', [I]);
  end;
end;

{ Object to JSONObject }
procedure TJsonDynamicConverter.ObjectToJSON(AObject: TObject;
  var AJSON: TJSONObject);
var
  Context: TRttiContext; // Contexto para RTTI
  RttiType: TRttiType; // Tipo RTTI do objeto
  Prop: TRttiProperty; // Propriedade atual do objeto
  PropValue: TValue; // Valor da propriedade atual
  SubJSON: TJSONObject; // Objeto JSON para propriedades que são objetos
  JSONArray: TJSONArray;
  // Array JSON para propriedades que são arrays dinâmicos
  I: integer; // Contador de loop
  DynArrayValue: TValue; // Valor de cada elemento do array dinâmico
begin
  // Cria o contexto RTTI para acessar informações sobre o tipo do objeto
  Context := TRttiContext.Create;
  try
    // Obtém o tipo RTTI do objeto passado como parâmetro
    RttiType := Context.GetType(AObject.ClassType);

    // Itera pelas propriedades do objeto utilizando RTTI
    for Prop in RttiType.GetProperties do
    begin
      // Verifica se a propriedade é legível (possui método getter)
      if Prop.IsReadable then
      begin
        // Obtém o valor atual da propriedade
        PropValue := Prop.GetValue(AObject);

        // Lida com propriedades que são arrays dinâmicos
        if (Prop.PropertyType.TypeKind = tkDynArray) then
        begin
          // Cria um novo array JSON para armazenar os elementos do array dinâmico
          JSONArray := TJSONArray.Create;

          // Itera pelos elementos do array dinâmico
          for I := 0 to PropValue.GetArrayLength - 1 do
          begin
            // Obtém o valor de cada elemento do array
            DynArrayValue := PropValue.GetArrayElement(I);
            // Verifica se o elemento é um objeto
            if DynArrayValue.IsObject then
            begin
              // Cria um novo objeto JSON para o elemento
              SubJSON := TJSONObject.Create;
              // Chama recursivamente a função para converter o objeto em JSON
              ObjectToJSON(DynArrayValue.AsObject, SubJSON);
              // Adiciona o objeto JSON ao array JSON
              JSONArray.AddElement(SubJSON);
            end
            // Verifica se o elemento é um número inteiro
            else if DynArrayValue.Kind = tkInteger then
              JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsInteger))
              // Verifica se o elemento é um número em ponto flutuante
            else if DynArrayValue.Kind = tkFloat then
              JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsExtended))
              // Verifica se o elemento é uma string
            else if DynArrayValue.Kind = tkString then
              JSONArray.AddElement(TJSONString.Create(DynArrayValue.AsString));
          end;

          // Adiciona o array JSON à propriedade correspondente no objeto JSON final
          AJSON.AddPair(Prop.Name, JSONArray);
        end
        // Lida com propriedades que são objetos aninhados
        else if PropValue.IsObject then
        begin
          // Cria um novo objeto JSON para a propriedade aninhada
          SubJSON := TJSONObject.Create;
          // Chama recursivamente a função para converter o objeto em JSON
          ObjectToJSON(PropValue.AsObject, SubJSON);
          // Adiciona o objeto JSON aninhado ao objeto JSON final
          AJSON.AddPair(Prop.Name, SubJSON);
        end
        // Lida com propriedades que são valores simples
        else if PropValue.Kind = tkInteger then
          AJSON.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsInteger))
        else if PropValue.Kind = tkFloat then
          AJSON.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsExtended))
        else if PropValue.Kind = tkUString then
          AJSON.AddPair(Prop.Name, TJSONString.Create(PropValue.AsString))
        else if PropValue.Kind = tkEnumeration then
        begin
          // Lida com valores booleanos
          if PropValue.TypeInfo = TypeInfo(Boolean) then
            AJSON.AddPair(Prop.Name, TJSONBool.Create(PropValue.AsBoolean))
          else
            AJSON.AddPair(Prop.Name, TJSONString.Create(PropValue.ToString));
        end
        // Lida com registros (como TDate e TDateTime)
        else if PropValue.Kind = tkRecord then
        begin
          // Verifica se a propriedade é do tipo TDate e a converte
          if Prop.PropertyType.Handle = TypeInfo(TDate) then
            AJSON.AddPair(Prop.Name,
              TJSONString.Create(DateToISO8601(PropValue.AsType<TDate>, False)))
            // Verifica se a propriedade é do tipo TDateTime e a converte
          else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
            AJSON.AddPair(Prop.Name,
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
procedure TJsonDynamicConverter.ObjectListToJSONArray<T>
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
    ObjectToJSON(AObjectList[I], SubJSON); // Converte o objeto para JSON
    AJSONArray.AddElement(SubJSON); // Adiciona o objeto JSON ao array
  end;
end;

{ UTILS }
procedure TJsonDynamicConverter.CheckValueType(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
begin
  // Verifica se o valor do campo no JSON é null
  if JsonPair.JsonValue.Null then
    Prop.SetValue(AObject, TValue.Empty) // Atribui um valor vazio
  else if JsonPair.JsonValue is TJSONNumber then
  begin
    // Verifica se a propriedade é do tipo inteiro
    if Prop.PropertyType.Handle = TypeInfo(integer) then
      Prop.SetValue(AObject,
        TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
      // Verifica se a propriedade é do tipo double (decimal)
    else if Prop.PropertyType.Handle = TypeInfo(double) then
      Prop.SetValue(AObject, TValue.From<double>(TJSONNumber(JsonPair.JsonValue)
        .AsDouble))
    else
      raise Exception.CreateFmt
        ('Tipo numérico não suportado para a propriedade "%s".', [Prop.Name]);
  end
  else if JsonPair.JsonValue is TJSONTrue then
    Prop.SetValue(AObject, TValue.From<Boolean>(True))
    // Atribui valor booleano True
  else if JsonPair.JsonValue is TJSONFalse then
    Prop.SetValue(AObject, TValue.From<Boolean>(False))
    // Atribui valor booleano False
  else if JsonPair.JsonValue is TJSONString then
  begin
    // Verifica se a propriedade é uma data
    if Prop.PropertyType.Handle = TypeInfo(TDate) then
      Prop.SetValue(AObject,
        TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue)
        .Value, False)))
      // Verifica se a propriedade é um DateTime
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
      ('Tipo de JSON não suportado para a propriedade "%s".', [Prop.Name]);
  // Erro se o tipo JSON não for suportado
end;

end.
