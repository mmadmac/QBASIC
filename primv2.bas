Screen _NewImage(1366, 980, 32)
Randomize Timer
Dim Shared laberinto(20, 20) As String
Dim Shared row, col, maxloops, currentloop As Integer
'Dim Shared celdasvisitadas(200) As Zeldas
Const True = 1
Const False = 0
Const Pared = "#"
Const Camino = " "

Cls

Type Zeldas
    x As Integer
    y As Integer

End Type

inicialaberinto

crealaberinto

pintalaberinto

End

ErrorVisitables:
Resume Next

'Iniciamos el laberinto a todo lleno.
Sub inicialaberinto
    row = 10 'Int(Rnd * 17) + 1
    col = 10 'Int(Rnd * 17) + 1
    maxloops = 1000
    currentloop = 0
    For x = 1 To 20
        For y = 1 To 20
            laberinto(x, y) = Pared
        Next
    Next
    laberinto(row, col) = Camino 'Selecciona la celda origen

    '   celdasvisitadas(0).x = row
    '   celdasvisitadas(0).y = col


End Sub

'Vamos excavando un camino.
Sub crealaberinto
    'Int(Rnd * 4)    '0 a 3
    RNDX = Int(Rnd * 10) * 2
    RNDY = Int(Rnd * 10) * 2

    'If hayVisitables(RNDX, RNDY) Then Print "Y"; Else Print "N";
    'Print RNDX, RNDY
    'Print Str$(celdasvisitadas(0).x) + " " + Str$(celdasvisitadas(0).y) + " " + Str$(celdasvisitadas(0).visitada)

    If hayVisitables(RNDX, RNDY) And laberinto(RNDX, RNDY) = Camino Then 'Taladras en direccion aleatoria donde haya una pared a dos casillas.

        'Taladrar el boquete en la direccion elegida
        Select Case Eligeunavisitable(RNDX, RNDY)
            Case 1
                laberinto(RNDX + 2, RNDY) = Camino
                laberinto(RNDX + 1, RNDY) = Camino
            Case 2
                laberinto(RNDX - 2, RNDY) = Camino
                laberinto(RNDX - 1, RNDY) = Camino
            Case 3
                laberinto(RNDX, RNDY + 2) = Camino
                laberinto(RNDX, RNDY + 1) = Camino
            Case 4
                laberinto(RNDX, RNDY - 2) = Camino
                laberinto(RNDX, RNDY - 1) = Camino

        End Select

    End If

    If currentloop >= maxloops Then Exit Sub
    currentloop = currentloop + 1

    crealaberinto

End Sub

'Y lo pintamos, lo m�s facil.
Sub pintalaberinto
    Dim filalaberinto As String
    'laberinto(row, col) = "o" 'pinta la ultima celda
    For y = 1 To 20
        For x = 1 To 20
            filalaberinto = filalaberinto + laberinto(x, y)
        Next
        Print filalaberinto
        filalaberinto = ""
    Next
End Sub

Function hayVisitables (celdax As Integer, celday As Integer)
    On Error GoTo ErrorVisitables

    hayVisitables = 0
    If laberinto(celdax + 2, celday) = Pared Then hayVisitables = True: Exit Function
    If laberinto(celdax - 2, celday) = Pared Then hayVisitables = True: Exit Function
    If laberinto(celdax, celday + 2) = Pared Then hayVisitables = True: Exit Function
    If laberinto(celdax, celday - 2) = Pared Then hayVisitables = True: Exit Function

    On Error GoTo 0

End Function

Function Eligeunavisitable (celdax As Integer, celday As Integer)

    elegimos:

    Select Case Int(Rnd * 4)
        Case 0
            If laberinto(celdax + 2, celday) = Pared Then pvisita = 1 'e
        Case 1
            If laberinto(celdax - 2, celday) = Pared Then pvisita = 2 'w
        Case 2
            If laberinto(celdax, celday + 2) = Pared Then pvisita = 3 's
        Case 3
            If laberinto(celdax, celday - 2) = Pared Then pvisita = 4 'n
    End Select
    If pvisita = 0 Then GoTo elegimos

    Eligeunavisitable = pvisita

End Function

