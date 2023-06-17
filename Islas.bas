Screen _NewImage(1024, 768, 32)
Randomize Timer
Dim Shared laberinto(20, 20) As String
Dim Shared row, col, maxloops, currentloop As Integer

inicialaberinto

crealaberinto

pintalaberinto


'Iniciamos el laberinto a todo lleno.
Sub inicialaberinto
    row = Int(Rnd * 17) + 1
    col = Int(Rnd * 17) + 1
    maxloops = 200
    currentloop = 0
    For x = 1 To 20
        For y = 1 To 20
            laberinto(x, y) = "#"
        Next
    Next
    laberinto(row, col) = "O"
End Sub

'Vamos excavando un camino desde el inicio hasta que nos salimos.
Sub crealaberinto
    'Int(Rnd * 4)    '0 a 3

    Select Case Int(Rnd * 4)
        Case 0
            If laberinto(row, col - 1) = "#" Then
                laberinto(row, col - 1) = " "
                col = col - 1
                If col <= 1 Then col = 1: Exit Select
            End If

        Case 1
            If laberinto(row + 1, col) = "#" Then
                laberinto(row + 1, col) = " "
                row = row + 1
                If row >= 19 Then row = 19: Exit Select
            End If

        Case 2
            If laberinto(row, col + 1) = "#" Then
                laberinto(row, col + 1) = " "
                col = col + 1
                If col >= 19 Then col = 19: Exit Select
            End If

        Case 3
            If laberinto(row - 1, col) = "#" Then
                laberinto(row - 1, col) = " "
                row = row - 1
                If row <= 1 Then row = 1: Exit Select
            End If

    End Select

    If currentloop >= maxloops Then Exit Sub
    currentloop = currentloop + 1

    crealaberinto
End Sub

'Y lo pintamos, lo má´s facil.
Sub pintalaberinto
    Dim filalaberinto As String
    laberinto(row, col) = "o"
    For y = 1 To 20
        For x = 1 To 20
            filalaberinto = filalaberinto + laberinto(x, y)
        Next
        Print filalaberinto
        filalaberinto = ""
    Next
End Sub

