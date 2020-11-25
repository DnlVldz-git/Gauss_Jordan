Sub Gauss()

    Dim matriz(2, 3) As Double 'se crea a matriz
    
    filas = 2 'se declaran los límites
    columnas = 3
    
    matriz(0, 0) = 3 'se inicializa la matriz
    matriz(0, 1) = -0.1
    matriz(0, 2) = -0.2
    matriz(0, 3) = 7.85
    matriz(1, 0) = 0.1
    matriz(1, 1) = 7
    matriz(1, 2) = -0.3
    matriz(1, 3) = -19.3
    matriz(2, 0) = 0.3
    matriz(2, 1) = -0.2
    matriz(2, 2) = 10
    matriz(2, 3) = 71.4
    
    Debug.Print ("Matriz original")
    
    'Se imprime la matriz original
    
    Debug.Print (matriz(0, 0) & "  " & matriz(0, 1) & " " & matriz(0, 2) & "  " & matriz(0, 3))
    Debug.Print (matriz(1, 0) & "  " & matriz(1, 1) & "  " & matriz(1, 2) & " " & matriz(1, 3))
    Debug.Print (matriz(2, 0) & "  " & matriz(2, 1) & "  " & matriz(2, 2) & "  " & matriz(2, 3))
    
    For pivote_fila = 0 To filas 'primer for que irá de arriba a abajo de la matriz
        normalizador = matriz(pivote_fila, pivote_fila) ' se selecciona el elemento que está en la diagonal, que servirá para normalizar ese mismo elemento
        
        For i = 0 To columnas ' segundo for para normalizar toda la fila
            matriz(pivote_fila, i) = matriz(pivote_fila, i) / normalizador 'se divide
        Next i
        f = pivote_fila + 1 'f será se encargará de hacer 0 los elementos en la misma columna del elemento en la diagonal
        If f = filas + 1 Then f = 0 'si está fuera del limite, es decir, es la diagonal inferior de la matriz, f será 0
        
        For fila = 0 To (filas - 1) 'estos fors anidados servirán para hacer 0 los elementos de la matriz en cada columna
            k = matriz(f, pivote_fila)  'se toma un valor k, es cual es está en la misma columna de el pivote de fila
            For c = pivote_fila To columnas 'se empieza desde el pivote fila, para no afectar la diagonal
                matriz(f, c) = matriz(f, c) - (k * matriz(pivote_fila, c))  ' se hace la operación en las filas menos en la fila pivote
            Next c
            
            If f = (filas) Then 'si f ha llegado al límite, se reiniciará
                f = 0
            Else  'en otro caso se incrementará
                f = f + 1
            End If
        Next fila
        
    Next pivote_fila
     
    Debug.Print ("Resuelta") ' se resuelve la matriz
     
    Debug.Print (matriz(0, 0) & "  " & matriz(0, 1) & " " & matriz(0, 2) & "  " & matriz(0, 3))
    Debug.Print (matriz(1, 0) & "  " & matriz(1, 1) & "  " & matriz(1, 2) & " " & matriz(1, 3))
    Debug.Print (matriz(2, 0) & "  " & matriz(2, 1) & "  " & matriz(2, 2) & "  " & matriz(2, 3))
    
    
End Sub

'funcion se trató de implementar pero no funcionó, se decidió implementar en el main
Function gauss_jordan(matriz() As Double, filas As Integer, columnas As Integer)
    For pivote_fila = 0 To filas
        normalizador = matriz(pivote_fila, pivote_fila)
        For i = 0 To columnas
            matriz(pivote_fila, i) = matriz(pivote_fila, i) / normalizador
        Next i
        f = pivote_fila + 1
        If f = filas Then f = 0
        
        For fila = 0 To filas - 1
            k = matriz(f, pivote_fila)
            For c = pivote_fila = 0 To columnas
                matriz(f, c) = matriz(f, c) - (k * matriz(pivote_f, c))
            Next c
            If f = filas - 1 Then f = 0
            Else: f = f + 1
        Next fila
    Next pivote_fila
    gauss_jordan = matriz
End Function
