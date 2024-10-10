### Procedimento: `TSomeClass.JSONToObject`

Este procedimento mapeia os campos de um `TJSONObject` para as propriedades correspondentes de um objeto Delphi (`TObject`). Ele usa **RTTI (Run-Time Type Information)** para acessar dinamicamente as propriedades do objeto e atribuir valores do JSON, suportando tipos simples (como inteiros, strings e datas) e tipos complexos (como matrizes dinâmicas).

#### Parâmetros:
- `AObject: TObject`: A instância do objeto para a qual os dados JSON serão mapeados.
- `AJSON: TJSONObject`: O objeto JSON contendo os dados que preencherão `AObject`.

---

### Documentação detalhada passo a passo:

1. **Inicialização do contexto RTTI**
```pascal
Context := TRttiContext.Create;
```
- O **`TRttiContext`** é criado para acessar metadados sobre o objeto em tempo de execução. Ele nos permite recuperar informações sobre as propriedades e métodos do objeto.

2. **Iterando por pares JSON**
```pascal
for JsonPair in AJSON do
```
- Este loop itera sobre cada par de chave-valor no `TJSONObject` fornecido. Cada `JsonPair` consiste em uma chave (`JsonString`) e um valor (`JsonValue`).

3. **Obter propriedade do objeto**
```pascal
Prop := RttiType.GetProperty(JsonPair.JsonString.Value);
```
- Para cada chave JSON, a propriedade correspondente do objeto (`AObject`) é recuperada usando RTTI. `RttiType` representa o tipo de classe do objeto, e `GetProperty` procura um nome de propriedade correspondente.

4. **Manipulando Arrays Dinâmicos**
```pascal
if (Prop.PropertyType.TypeKind = tkDynArray) and (JsonPair.JsonValue is TJSONArray) then
```
- Se a propriedade for um array dinâmico e o valor JSON for um array JSON (`TJSONArray`), o procedimento prossegue com o manuseio de arrays.

- **Manipulando Array**:
1. **Inicialize o Array**:
```pascal
SetLength(TValueArray, JsonArray.Count);
```
O procedimento cria um array (`TValueArray`) de `TValue` com o mesmo tamanho do array JSON.

2. **Determine Array Element Class**:
```pascal
ElemClass := (Prop.PropertyType as TRttiDynamicArrayType).ElementType.AsInstance.MetaclassType;
```
RTTI é usado para recuperar a classe dos elementos no array dinâmico. Isso permite que o procedimento crie objetos dinamicamente para cada elemento do array.

3. **Preencher Array**:
```pascal
SubObject := ElemClass.Create;
JSONToObject(SubObject, JsonArray.Items[I] as TJSONObject);
TValueArray[I] := TValue.From(SubObject);```
Para cada item na matriz JSON:
- Uma nova instância da classe de elemento (`SubObject`) é criada.
- O procedimento chama recursivamente `JSONToObject` para preencher o objeto aninhado com dados JSON.
- O objeto é então adicionado a `TValueArray` como um `TValue`.

4. **Atribuir matriz à propriedade do objeto**:
```pascal
ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
Prop.SetValue(AObject, ArrayValue);
```
A matriz dinâmica é convertida em `TValue` e atribuída à propriedade do objeto (`AObject`).

5. **Manipulando tipos simples (strings, números, booleanos)**
- Se a propriedade não for uma matriz, o procedimento manipula tipos de dados simples como strings, inteiros, booleanos e datas.

- **Valores Nulos**:
```pascal
if JsonPair.JsonValue.Null then
Prop.SetValue(AObject, TValue.Empty);
```
Se o valor JSON for nulo, a propriedade será definida como um valor vazio.

- **Números**:
```pascal
if Prop.PropertyType.Handle = TypeInfo(integer) then
Prop.SetValue(AObject, TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
```
Se a propriedade for do tipo `integer` ou `double`, o número JSON será convertido e atribuído.

- **Booleans**:
```pascal
if JsonPair.JsonValue is TJSONTrue then
Prop.SetValue(AObject, TValue.From<Boolean>(True))
```
Os valores JSON `true` e `false` são atribuídos como booleanos.

- **Strings**:
```pascal
if JsonPair.JsonValue is TJSONString then
Prop.SetValue(AObject, TValue.From<string>(TJSONString(JsonPair.JsonValue).Value))
```
Se o valor for uma string, ele será atribuído diretamente à propriedade.

- **Datas**:
```pascal
if Prop.PropertyType.Handle = TypeInfo(TDate) then
Prop.SetValue(AObject, TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue).Value, False)))
```
Strings que representam datas são convertidas para `TDate` ou `TDateTime` usando a função `ISO8601ToDate` e atribuídas.

6. **Tratamento de tipos não suportados**
- Se o valor JSON não corresponder a um tipo suportado, uma exceção será gerada:
```pascal
raise Exception.CreateFmt('Tipo de JSON não suportado para a propriedade "%s".', [Prop.Name]);
```

7. **Limpeza de contexto RTTI**
```pascal
finally
Context.Free;
end;
```
- O contexto RTTI é liberado para liberar recursos de memória assim que o procedimento for concluído.

---

### Pontos chaves a considerar para manutenção:
- **Recursão**: A função é recursiva para objetos JSON aninhados, tornando-a capaz de manipular estruturas de dados complexas.
- **Manipulação de matrizes**: Matrizes dinâmicas exigem manipulação especial devido à sua natureza complexa. Certifique-se de que `ElemClass` seja identificado corretamente para todos os elementos da matriz.
- **Manipulação de exceções**: O procedimento inclui manipulação de exceções para gerenciar estruturas JSON inválidas e tipos de propriedade inesperados.
- **Tipos suportados**: A versão atual suporta inteiros, duplos, booleanos, strings, datas e matrizes dinâmicas. Se você precisar suportar tipos adicionais, estenda a lógica de verificação de tipos nas seções apropriadas.
- **Desempenho do RTTI**: Embora o RTTI seja poderoso, ele pode introduzir sobrecarga de desempenho em grandes conjuntos de dados. Considere otimizar, se necessário.

Seguindo esta documentação, modificações futuras devem ser mais fáceis, pois cada seção explica claramente sua finalidade e comportamento.