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
    class procedure JSONStringToObject(AJSONString: string; AObject: TObject);

    // JSONObject to Object
    class procedure JSONToObject(AJSONObject: TJSONObject; AObject: TObject);

    // JSONString to ObjectList
    class procedure JSONStringToObjectList<T: class, constructor>(AJSONString: string;
      AObjectList: TObjectList<T>);

    // JSONArray to ObjectList
    class procedure JSONArrayToObjectList<T: class, constructor>
      (AJSONArray: TJSONArray; AObjectList: TObjectList<T>);

    { ---------------------- OBJECT TO JSON ---------------------- }

    // Object to JSONObject
    class procedure ObjectToJSONObject(AObject: TObject;
      var AJSONObject: TJSONObject);

    // ObjectList to JSONArray
    class procedure ObjectListToJSONArray<T: class>(AObjectList: TObjectList<T>;
      var AJSONArray: TJSONArray);

  private
    { --------------------------- UTILS --------------------------- }

    // Check Json Value Type
    class procedure CheckJSONValueType(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject);
    class procedure HandleDynamicArray(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject); static;
    class procedure HandleObject(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject); static;
    class procedure HandleDynamicArrayToJSON(Prop: TRttiProperty;
      PropValue: TValue; var AJSONObject: TJSONObject); static;
    class procedure HandleObjectToJSON(Prop: TRttiProperty; PropValue: TValue;
      var AJSONObject: TJSONObject); static;
    class procedure HandleSimpleValueToJSON(Prop: TRttiProperty;
      PropValue: TValue; var AJSONObject: TJSONObject); static;
  end;

implementation

{ --------------------------- JSON TO OBJECT --------------------------- }

{ JSONString to Object }
class procedure TJSONDynConverter.JSONStringToObject(AJSONString: string;
  AObject: TObject);
begin
  // Faz o parsing da string JSON para um objeto JSON (TJSONObject)
  var AJSONObject := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
  try
    // Chama a fun��o para converter o objeto JSON para o objeto Delphi
    JSONToObject(AJSONObject, AObject);
  finally
    // Libera a mem�ria do objeto JSON ap�s o uso
    AJSONObject.Free;
  end;
end;

{ JSONObject to Object }
class procedure TJSONDynConverter.JSONToObject(AJSONObject: TJSONObject;
  AObject: TObject);
var
  Context: TRttiContext; // Contexto RTTI para acessar as propriedades do objeto
  RttiType: TRttiType; // Representa o tipo do objeto usando RTTI
  Prop: TRttiProperty; // Propriedade RTTI para cada campo no objeto
  JsonPair: TJSONPair; // Representa um par de chave-valor no JSON
begin
  // Inicializa o contexto RTTI
  Context := TRttiContext.Create;
  try
    // Obt�m o tipo RTTI do objeto
    RttiType := Context.GetType(AObject.ClassType);

    // Itera sobre cada par chave-valor do JSON
    for JsonPair in AJSONObject do
    begin
      // Tenta obter a propriedade do objeto correspondente � chave do JSON
      Prop := RttiType.GetProperty(JsonPair.JSONString.Value);

      // Verifica se a propriedade existe no objeto
      if Assigned(Prop) then
      begin
        // Verifica se a propriedade � um array din�mico e o valor no JSON � um array
        if (Prop.PropertyType.TypeKind = tkDynArray) and (JsonPair.JsonValue is TJSONArray) then
          // Chama a fun��o para lidar com arrays din�micos
          HandleDynamicArray(JsonPair, Prop, AObject)
        // Verifica se o valor no JSON � um objeto aninhado
        else if JsonPair.JsonValue is TJSONObject then
          // Chama a fun��o para lidar com objetos aninhados
          HandleObject(JsonPair, Prop, AObject)
        else
          // Chama a fun��o para lidar com tipos simples (strings, n�meros, etc.)
          CheckJSONValueType(JsonPair, Prop, AObject);
      end;
    end;
  finally
    // Libera o contexto RTTI ao final
    Context.Free;
  end;
end;

{ JSONString to ObjectList }
class procedure TJSONDynConverter.JSONStringToObjectList<T>(AJSONString: string;
  AObjectList: TObjectList<T>);
var
  I: integer;       // Vari�vel para o loop
  SubObject: T;     // Vari�vel para armazenar cada objeto criado a partir do JSON
begin
  // Faz o parsing da string JSON para um array JSON (TJSONArray)
  var AJSONArray := TJSONObject.ParseJSONValue(AJSONString) as TJSONArray;
  try
    // Converte o array JSON em uma lista de objetos
    JSONArrayToObjectList<T>(AJSONArray, AObjectList);
  finally
    // Libera a mem�ria do array JSON ap�s o uso
    AJSONArray.Free;
  end;
end;

{ JSONArray to ObjectList }
class procedure TJSONDynConverter.JSONArrayToObjectList<T>(AJSONArray: TJSONArray;
  AObjectList: TObjectList<T>);
var
  I: integer;       // Vari�vel para iterar pelos itens do array
  SubObject: T;     // Vari�vel para armazenar o objeto criado para cada item do array
begin
  // Limpa a lista para garantir que estar� vazia antes de adicionar novos objetos
  AObjectList.Clear;

  // Itera sobre cada item do array JSON
  for I := 0 to AJSONArray.Count - 1 do
  begin
    // Verifica se o item atual do array � um objeto JSON (TJSONObject)
    if AJSONArray.Items[I] is TJSONObject then
    begin
      // Cria um novo objeto do tipo gen�rico T
      SubObject := T.Create;
      try
        // Converte o item JSON para o objeto Delphi utilizando JSONToObject
        JSONToObject(AJSONArray.Items[I] as TJSONObject, SubObject);
        // Adiciona o objeto convertido � lista de objetos
        AObjectList.Add(SubObject);
      except
        // Libera a mem�ria do objeto caso ocorra algum erro durante a convers�o
        SubObject.Free;
        // Relan�a a exce��o para o tratamento apropriado
        raise;
      end;
    end
    else
      // Lan�a uma exce��o se o item no array n�o for um TJSONObject
      raise Exception.CreateFmt
        ('Item no �ndice %d n�o � um objeto JSON v�lido.', [I]);
  end;
end;

{ --------------------------- OBJECT TO JSON --------------------------- }

{ Object to JSONObject }
class procedure TJSONDynConverter.ObjectToJSONObject(AObject: TObject;
  var AJSONObject: TJSONObject);
var
  Context: TRttiContext; // Contexto RTTI para acessar metadados do objeto
  RttiType: TRttiType; // Tipo RTTI do objeto atual
  Prop: TRttiProperty; // Propriedade do objeto sendo iterada
  PropValue: TValue; // Valor da propriedade sendo processada
begin
  // Cria um contexto RTTI para acessar informa��es sobre o objeto
  Context := TRttiContext.Create;
  try
    // Obt�m o tipo RTTI do objeto passado (classe do objeto)
    RttiType := Context.GetType(AObject.ClassType);

    // Itera pelas propriedades do objeto utilizando RTTI
    for Prop in RttiType.GetProperties do
    begin
      // Verifica se a propriedade tem um m�todo getter (� leg�vel)
      if Prop.IsReadable then
      begin
        // Obt�m o valor atual da propriedade
        PropValue := Prop.GetValue(AObject);

        // Verifica se a propriedade � um array din�mico
        if Prop.PropertyType.TypeKind = tkDynArray then
          // Chama fun��o para lidar com arrays din�micos
          HandleDynamicArrayToJSON(Prop, PropValue, AJSONObject)
        // Verifica se a propriedade � um objeto
        else if PropValue.IsObject then
          // Chama fun��o para lidar com objetos aninhados
          HandleObjectToJSON(Prop, PropValue, AJSONObject)
        // Caso contr�rio, lida com propriedades simples (inteiros, strings, etc.)
        else
          HandleSimpleValueToJSON(Prop, PropValue, AJSONObject);
      end;
    end;
  finally
    // Libera o contexto RTTI ap�s o processamento
    Context.Free;
  end;
end;

{ ObjectList to JSONArray }
class procedure TJSONDynConverter.ObjectListToJSONArray<T>
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

{ -------------------------------- UTILS ------------------------------- }

{ --------------------------- JSON TO OBJECT --------------------------- }

{ HandleObject }
class procedure TJSONDynConverter.HandleObject(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
var
  SubObject: TObject; // Objeto auxiliar para instanciar a classe aninhada
begin
  // Cria uma inst�ncia do tipo do objeto aninhado
  SubObject := Prop.PropertyType.AsInstance.MetaclassType.Create;
  try
    // Chama recursivamente para preencher o subobjeto com o JSON
    JSONToObject(JsonPair.JsonValue as TJSONObject, SubObject);
    // Atribui o subobjeto � propriedade do objeto principal
    Prop.SetValue(AObject, SubObject);
  except
    // Libera o subobjeto em caso de erro
    SubObject.Free;
    raise;
  end;
end;

{ HandleDynamicArray }
class procedure TJSONDynConverter.HandleDynamicArray(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
var
  JSONArray: TJSONArray; // Array no JSON
  ElemClass: TClass; // Classe dos elementos do array din�mico
  I: integer; // Iterador do loop
  SubObject: TObject; // Objeto auxiliar para criar inst�ncias dos elementos do array
  TValueArray: TArray<TValue>; // Array de valores para os elementos do array din�mico
  ArrayValue: TValue; // Valor final que ser� atribu�do � propriedade do objeto
begin
  // Converte o valor JSON para TJSONArray
  JSONArray := TJSONArray(JsonPair.JsonValue);
  // Define o tamanho do array din�mico com base no tamanho do array JSON
  SetLength(TValueArray, JSONArray.Count);

  // Obt�m a classe dos elementos do array din�mico
  ElemClass := (Prop.PropertyType as TRttiDynamicArrayType).ElementType.AsInstance.MetaclassType;

  // Itera sobre os itens do array no JSON
  for I := 0 to JSONArray.Count - 1 do
  begin
    // Verifica se o item do array no JSON � um objeto JSON
    if JSONArray.Items[I] is TJSONObject then
    begin
      // Cria uma inst�ncia do tipo de elemento do array
      SubObject := ElemClass.Create;
      try
        // Chama recursivamente para preencher o subobjeto com o JSON
        JSONToObject(JSONArray.Items[I] as TJSONObject, SubObject);
        // Armazena o subobjeto no array de valores
        TValueArray[I] := TValue.From(SubObject);
      except
        // Libera o subobjeto em caso de erro
        SubObject.Free;
        raise;
      end;
    end
    else
      // Erro se o item do array n�o for um objeto JSON
      raise Exception.CreateFmt('Array item at index %d is not a valid JSON object.', [I]);
  end;

  // Converte o array de valores para um TValue e o atribui � propriedade do objeto
  ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
  Prop.SetValue(AObject, ArrayValue);
end;

{ CheckValueType }
class procedure TJSONDynConverter.CheckJSONValueType(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
begin
  // Verifica se o valor do campo no JSON � null
  if JsonPair.JsonValue.Null then
    // Atribui um valor vazio se o JSON contiver um valor nulo
    Prop.SetValue(AObject, TValue.Empty)
  else if JsonPair.JsonValue is TJSONNumber then
  begin
    // Verifica se a propriedade � um n�mero inteiro
    if Prop.PropertyType.Handle = TypeInfo(integer) then
      Prop.SetValue(AObject, TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
    // Verifica se a propriedade � um n�mero decimal
    else if Prop.PropertyType.Handle = TypeInfo(double) then
      Prop.SetValue(AObject, TValue.From<double>(TJSONNumber(JsonPair.JsonValue).AsDouble))
    else
      // Gera um erro se o tipo num�rico n�o for suportado
      raise Exception.CreateFmt('Tipo num�rico n�o suportado para a propriedade "%s".', [Prop.Name]);
  end
  else if JsonPair.JsonValue is TJSONTrue then
    // Atribui o valor booleano True
    Prop.SetValue(AObject, TValue.From<Boolean>(True))
  else if JsonPair.JsonValue is TJSONFalse then
    // Atribui o valor booleano False
    Prop.SetValue(AObject, TValue.From<Boolean>(False))
  else if JsonPair.JsonValue is TJSONString then
  begin
    // Verifica se a propriedade � uma data
    if Prop.PropertyType.Handle = TypeInfo(TDate) then
      Prop.SetValue(AObject, TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue).Value, False)))
    // Verifica se a propriedade � um DateTime
    else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
      Prop.SetValue(AObject, TValue.From<TDateTime>(ISO8601ToDate(TJSONString(JsonPair.JsonValue).Value, True)))
    else
      // Atribui o valor como string
      Prop.SetValue(AObject, TValue.From<string>(TJSONString(JsonPair.JsonValue).Value));
  end
  else
    // Gera um erro se o tipo JSON n�o for suportado
    raise Exception.CreateFmt('Tipo de JSON n�o suportado para a propriedade "%s".', [Prop.Name]);
end;

{ --------------------------- OBJECT TO JSON --------------------------- }

{ HandleObjectToJSON }
class procedure TJSONDynConverter.HandleObjectToJSON(Prop: TRttiProperty;
  PropValue: TValue; var AJSONObject: TJSONObject);
var
  SubJSON: TJSONObject; // Subobjeto JSON para armazenar o valor do objeto aninhado
begin
  // Cria um objeto JSON para armazenar o conte�do do objeto aninhado
  SubJSON := TJSONObject.Create;
  // Converte o objeto aninhado chamando recursivamente ObjectToJSONObject
  ObjectToJSONObject(PropValue.AsObject, SubJSON);
  // Adiciona o subobjeto JSON ao objeto JSON principal
  AJSONObject.AddPair(Prop.Name, SubJSON);
end;

{ HandleDynamicArrayToJSON }
class procedure TJSONDynConverter.HandleDynamicArrayToJSON(Prop: TRttiProperty;
  PropValue: TValue; var AJSONObject: TJSONObject);
var
  JSONArray: TJSONArray; // Array JSON para armazenar os valores
  I: integer; // Contador de loop para iterar sobre os elementos do array
  DynArrayValue: TValue; // Valor de cada elemento do array din�mico
  SubJSON: TJSONObject; // Subobjeto JSON para armazenar objetos dentro do array
begin
  // Cria um array JSON para armazenar os elementos do array din�mico
  JSONArray := TJSONArray.Create;

  // Itera pelos elementos do array din�mico
  for I := 0 to PropValue.GetArrayLength - 1 do
  begin
    // Obt�m o valor de cada elemento do array
    DynArrayValue := PropValue.GetArrayElement(I);

    // Verifica se o elemento do array � um objeto
    if DynArrayValue.IsObject then
    begin
      // Cria um objeto JSON para o elemento
      SubJSON := TJSONObject.Create;
      // Converte o objeto chamando recursivamente ObjectToJSONObject
      ObjectToJSONObject(DynArrayValue.AsObject, SubJSON);
      // Adiciona o objeto JSON ao array
      JSONArray.AddElement(SubJSON);
    end
    // Caso o elemento seja um inteiro
    else if DynArrayValue.Kind = tkInteger then
      JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsInteger))
    // Caso o elemento seja um n�mero de ponto flutuante
    else if DynArrayValue.Kind = tkFloat then
      JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsExtended))
    // Caso o elemento seja uma string
    else if DynArrayValue.Kind = tkString then
      JSONArray.AddElement(TJSONString.Create(DynArrayValue.AsString));
  end;

  // Adiciona o array JSON ao objeto JSON principal
  AJSONObject.AddPair(Prop.Name, JSONArray);
end;

{ HandleSimpleValueToJSON }
class procedure TJSONDynConverter.HandleSimpleValueToJSON(Prop: TRttiProperty;
  PropValue: TValue; var AJSONObject: TJSONObject);
begin
  // Caso o valor seja um n�mero inteiro
  if PropValue.Kind = tkInteger then
    AJSONObject.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsInteger))
  // Caso o valor seja um n�mero de ponto flutuante
  else if PropValue.Kind = tkFloat then
    AJSONObject.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsExtended))
  // Caso o valor seja uma string
  else if PropValue.Kind = tkUString then
    AJSONObject.AddPair(Prop.Name, TJSONString.Create(PropValue.AsString))
  // Lida com valores de enumera��o (como booleanos)
  else if PropValue.Kind = tkEnumeration then
  begin
    // Caso seja um valor booleano
    if PropValue.TypeInfo = TypeInfo(Boolean) then
      AJSONObject.AddPair(Prop.Name, TJSONBool.Create(PropValue.AsBoolean))
    else
      AJSONObject.AddPair(Prop.Name, TJSONString.Create(PropValue.ToString));
  end
  // Lida com tipos de registro, como TDate e TDateTime
  else if PropValue.Kind = tkRecord then
  begin
    // Caso a propriedade seja do tipo TDate
    if Prop.PropertyType.Handle = TypeInfo(TDate) then
      AJSONObject.AddPair(Prop.Name, TJSONString.Create(DateToISO8601(PropValue.AsType<TDate>, False)))
    // Caso a propriedade seja do tipo TDateTime
    else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
      AJSONObject.AddPair(Prop.Name, TJSONString.Create(DateToISO8601(PropValue.AsType<TDateTime>, True)));
  end;
end;

end.
