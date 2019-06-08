Public Class Matriz_3x3
    Private Sub RadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton1.CheckedChanged
        TxtAV1.Enabled = True
        TxtAV2.Enabled = True
        TxtAV3.Enabled = True
        TxtAV4.Enabled = False
        TxtAV5.Enabled = False
        TxtAW1.Enabled = True
        TxtAW2.Enabled = True
        TxtAW3.Enabled = True
        TxtAW4.Enabled = False
        TxtAW5.Enabled = False
        TxtAX1.Enabled = True
        TxtAX2.Enabled = True
        TxtAX3.Enabled = True
        TxtAX4.Enabled = False
        TxtAX5.Enabled = False
        TxtAC1.Enabled = True
        TxtAC2.Enabled = True
        TxtAC3.Enabled = True
        TxtAC4.Enabled = False
        TxtAC5.Enabled = False
        TxtAY1.Enabled = False
        TxtAY2.Enabled = False
        TxtAY3.Enabled = False
        TxtAY4.Enabled = False
        TxtAY5.Enabled = False
        TxtAZ1.Enabled = False
        TxtAZ2.Enabled = False
        TxtAZ3.Enabled = False
        TxtAZ4.Enabled = False
        TxtAZ5.Enabled = False
    End Sub
    Private Sub RadioButton2_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton2.CheckedChanged
        TxtAV1.Enabled = True
        TxtAV2.Enabled = True
        TxtAV3.Enabled = True
        TxtAV4.Enabled = True
        TxtAV5.Enabled = False
        TxtAW1.Enabled = True
        TxtAW2.Enabled = True
        TxtAW3.Enabled = True
        TxtAW4.Enabled = True
        TxtAW5.Enabled = False
        TxtAX1.Enabled = True
        TxtAX2.Enabled = True
        TxtAX3.Enabled = True
        TxtAX4.Enabled = True
        TxtAX5.Enabled = False
        TxtAC1.Enabled = True
        TxtAC2.Enabled = True
        TxtAC3.Enabled = True
        TxtAC4.Enabled = True
        TxtAC5.Enabled = False
        TxtAY1.Enabled = True
        TxtAY2.Enabled = True
        TxtAY3.Enabled = True
        TxtAY4.Enabled = True
        TxtAY5.Enabled = False
        TxtAZ1.Enabled = False
        TxtAZ2.Enabled = False
        TxtAZ3.Enabled = False
        TxtAZ4.Enabled = False
        TxtAZ5.Enabled = False
    End Sub
    Private Sub RadioButton3_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton3.CheckedChanged
        TxtAV1.Enabled = True
        TxtAV2.Enabled = True
        TxtAV3.Enabled = True
        TxtAV4.Enabled = True
        TxtAV5.Enabled = True
        TxtAW1.Enabled = True
        TxtAW2.Enabled = True
        TxtAW3.Enabled = True
        TxtAW4.Enabled = True
        TxtAW5.Enabled = True
        TxtAX1.Enabled = True
        TxtAX2.Enabled = True
        TxtAX3.Enabled = True
        TxtAX4.Enabled = True
        TxtAX5.Enabled = True
        TxtAC1.Enabled = True
        TxtAC2.Enabled = True
        TxtAC3.Enabled = True
        TxtAC4.Enabled = True
        TxtAC5.Enabled = True
        TxtAY1.Enabled = True
        TxtAY2.Enabled = True
        TxtAY3.Enabled = True
        TxtAY4.Enabled = True
        TxtAY5.Enabled = True
        TxtAZ1.Enabled = True
        TxtAZ2.Enabled = True
        TxtAZ3.Enabled = True
        TxtAZ4.Enabled = True
        TxtAZ5.Enabled = True
    End Sub
    Dim m11, m12, m13, m21, m22, m23, m31, m32, m33, c11, c12, c13, c21, c22, c23, c31, c32, c33 As Integer
    Dim A, ad11, ad12, ad13, ad21, ad22, ad23, ad31, ad32, ad33, ad14, ad24, ad34, ad41, ad42, ad43, ad44 As Integer
    Dim m41, m42, m43, m44, c41, c42, c43, c44, m14, m24, m34, c14, c24, c34 As Integer
    Dim b11, b12, b13, b21, b22, b23, b31, b32, b33, b14, b24, b34, b41, b42, b43, b44 As Double
    Dim z, x, y, v As Integer
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If RadioButton1.Checked = True Then
            If (TxtAV1.Text = 0 And TxtAW1.Text = 0 And TxtAX1.Text = 0) Or (TxtAV2.Text = 0 And TxtAW2.Text = 0 And TxtAX2.Text = 0) Or (TxtAV3.Text = 0 And TxtAW3.Text = 0 And TxtAX3.Text = 0) Then
                MessageBox.Show("No se puede resolver el sistema de ecuaciones porque no es cuadrado")
            Else
                m11 = (TxtAW2.Text * TxtAX3.Text) - (TxtAW3.Text * TxtAX2.Text)
                m12 = (TxtAV2.Text * TxtAX3.Text) - (TxtAV3.Text * TxtAX2.Text)
                m13 = (TxtAV2.Text * TxtAW3.Text) - (TxtAV3.Text * TxtAW2.Text)
                m21 = (TxtAW1.Text * TxtAX3.Text) - (TxtAW3.Text * TxtAX1.Text)
                m22 = (TxtAV1.Text * TxtAX3.Text) - (TxtAV3.Text * TxtAX1.Text)
                m23 = (TxtAV1.Text * TxtAW3.Text) - (TxtAV3.Text * TxtAW1.Text)
                m31 = (TxtAW1.Text * TxtAX2.Text) - (TxtAW2.Text * TxtAX1.Text)
                m32 = (TxtAV1.Text * TxtAX2.Text) - (TxtAV2.Text * TxtAX1.Text)
                m33 = (TxtAV1.Text * TxtAW2.Text) - (TxtAV2.Text * TxtAW1.Text)
                Label65.Text = m11 & "  " & m12 & "  " & m13 & vbCrLf & m21 & "  " & m22 & "  " & m23 & vbCrLf & m31 & "  " & m32 & "  " & m33
            End If
        ElseIf RadioButton2.Checked = True Then
            If (TxtAV1.Text = 0 And TxtAW1.Text = 0 And TxtAX1.Text = 0 And TxtAY1.Text = 0) Or (TxtAV2.Text = 0 And TxtAW2.Text = 0 And TxtAX2.Text = 0 And TxtAY2.Text = 0) Or (TxtAV3.Text = 0 And TxtAW3.Text = 0 And TxtAX3.Text = 0 And TxtAY3.Text = 0) Or (TxtAV4.Text = 0 And TxtAW4.Text = 0 And TxtAX4.Text = 0 And TxtAY4.Text = 0) Then
                MessageBox.Show("No se puede resolver el sistema de ecuaciones porque no es cuadrado")
            Else
                m11 = -((TxtAW4.Text * TxtAX3.Text * TxtAY2.Text) + (TxtAX4.Text * TxtAY3.Text * TxtAW2.Text) + (TxtAY4.Text * TxtAW3.Text * TxtAX2.Text)) + ((TxtAW2.Text * TxtAX3.Text * TxtAY4.Text) + (TxtAX2.Text * TxtAY3.Text * TxtAW4.Text) + (TxtAY2.Text * TxtAW3.Text * TxtAX4.Text))
                m12 = -((TxtAV4.Text * TxtAX3.Text * TxtAY2.Text) + (TxtAX4.Text * TxtAY3.Text * TxtAV2.Text) + (TxtAY4.Text * TxtAV3.Text * TxtAX2.Text)) + ((TxtAV2.Text * TxtAX3.Text * TxtAY4.Text) + (TxtAX2.Text * TxtAY3.Text * TxtAV4.Text) + (TxtAY2.Text * TxtAV3.Text * TxtAX4.Text))
                m13 = -((TxtAV4.Text * TxtAW3.Text * TxtAY2.Text) + (TxtAW4.Text * TxtAY3.Text * TxtAV2.Text) + (TxtAY4.Text * TxtAV3.Text * TxtAW2.Text)) + ((TxtAV2.Text * TxtAW3.Text * TxtAY4.Text) + (TxtAX2.Text * TxtAY3.Text * TxtAV4.Text) + (TxtAY2.Text * TxtAV3.Text * TxtAW4.Text))
                m14 = -((TxtAV4.Text * TxtAW3.Text * TxtAX2.Text) + (TxtAW4.Text * TxtAX3.Text * TxtAV2.Text) + (TxtAX4.Text * TxtAV3.Text * TxtAW2.Text)) + ((TxtAV2.Text * TxtAW3.Text * TxtAX4.Text) + (TxtAW2.Text * TxtAX3.Text * TxtAV4.Text) + (TxtAX2.Text * TxtAV3.Text * TxtAW4.Text))
                m21 = -((TxtAW4.Text * TxtAX3.Text * TxtAY1.Text) + (TxtAX4.Text * TxtAY3.Text * TxtAW1.Text) + (TxtAY4.Text * TxtAW3.Text * TxtAX1.Text)) + ((TxtAW1.Text * TxtAX3.Text * TxtAY4.Text) + (TxtAX1.Text * TxtAY3.Text * TxtAW4.Text) + (TxtAY1.Text * TxtAW3.Text * TxtAX4.Text))
                m22 = -((TxtAV4.Text * TxtAX3.Text * TxtAY1.Text) + (TxtAX4.Text * TxtAY3.Text * TxtAV1.Text) + (TxtAY4.Text * TxtAV3.Text * TxtAX1.Text)) + ((TxtAV1.Text * TxtAX3.Text * TxtAY4.Text) + (TxtAX1.Text * TxtAY3.Text * TxtAV4.Text) + (TxtAY1.Text * TxtAV3.Text * TxtAX4.Text))
                m23 = -((TxtAV4.Text * TxtAW3.Text * TxtAY1.Text) + (TxtAW4.Text * TxtAY3.Text * TxtAV1.Text) + (TxtAY4.Text * TxtAV3.Text * TxtAW1.Text)) + ((TxtAV1.Text * TxtAW3.Text * TxtAY4.Text) + (TxtAX1.Text * TxtAY3.Text * TxtAV4.Text) + (TxtAY1.Text * TxtAV3.Text * TxtAW4.Text))
                m24 = -((TxtAV4.Text * TxtAW3.Text * TxtAX1.Text) + (TxtAW4.Text * TxtAX3.Text * TxtAV1.Text) + (TxtAX4.Text * TxtAV3.Text * TxtAW1.Text)) + ((TxtAV1.Text * TxtAW3.Text * TxtAX4.Text) + (TxtAW1.Text * TxtAX3.Text * TxtAV4.Text) + (TxtAX1.Text * TxtAV3.Text * TxtAW4.Text))
                m31 = -((TxtAW4.Text * TxtAX2.Text * TxtAY1.Text) + (TxtAX4.Text * TxtAY2.Text * TxtAW1.Text) + (TxtAY4.Text * TxtAW2.Text * TxtAX1.Text)) + ((TxtAW1.Text * TxtAX2.Text * TxtAY4.Text) + (TxtAX1.Text * TxtAY2.Text * TxtAW4.Text) + (TxtAY1.Text * TxtAW2.Text * TxtAX4.Text))
                m32 = -((TxtAV4.Text * TxtAX2.Text * TxtAY1.Text) + (TxtAX4.Text * TxtAY2.Text * TxtAV1.Text) + (TxtAY4.Text * TxtAV2.Text * TxtAX1.Text)) + ((TxtAV1.Text * TxtAX2.Text * TxtAY4.Text) + (TxtAX1.Text * TxtAY2.Text * TxtAV4.Text) + (TxtAY1.Text * TxtAV2.Text * TxtAX4.Text))
                m33 = -((TxtAV4.Text * TxtAW2.Text * TxtAY1.Text) + (TxtAW4.Text * TxtAY2.Text * TxtAV1.Text) + (TxtAY4.Text * TxtAV2.Text * TxtAW1.Text)) + ((TxtAV1.Text * TxtAW2.Text * TxtAY4.Text) + (TxtAW1.Text * TxtAY2.Text * TxtAV4.Text) + (TxtAY1.Text * TxtAV2.Text * TxtAW4.Text))
                m34 = -((TxtAV4.Text * TxtAW2.Text * TxtAX1.Text) + (TxtAW4.Text * TxtAX2.Text * TxtAV1.Text) + (TxtAX4.Text * TxtAV2.Text * TxtAW1.Text)) + ((TxtAV1.Text * TxtAW2.Text * TxtAX4.Text) + (TxtAW1.Text * TxtAX2.Text * TxtAV4.Text) + (TxtAX1.Text * TxtAV2.Text * TxtAW4.Text))
                m41 = -((TxtAW3.Text * TxtAX2.Text * TxtAY1.Text) + (TxtAX3.Text * TxtAY2.Text * TxtAW1.Text) + (TxtAY3.Text * TxtAW2.Text * TxtAX1.Text)) + ((TxtAW1.Text * TxtAX2.Text * TxtAY3.Text) + (TxtAX1.Text * TxtAY2.Text * TxtAW3.Text) + (TxtAY1.Text * TxtAW2.Text * TxtAX3.Text))
                m42 = -((TxtAV3.Text * TxtAX2.Text * TxtAY1.Text) + (TxtAX3.Text * TxtAY2.Text * TxtAV1.Text) + (TxtAY3.Text * TxtAV2.Text * TxtAX1.Text)) + ((TxtAV1.Text * TxtAX2.Text * TxtAY3.Text) + (TxtAX1.Text * TxtAY2.Text * TxtAV3.Text) + (TxtAY1.Text * TxtAV2.Text * TxtAX3.Text))
                m43 = -((TxtAV3.Text * TxtAW2.Text * TxtAY1.Text) + (TxtAW3.Text * TxtAY2.Text * TxtAV1.Text) + (TxtAY3.Text * TxtAV2.Text * TxtAW1.Text)) + ((TxtAV1.Text * TxtAW2.Text * TxtAY3.Text) + (TxtAX1.Text * TxtAY2.Text * TxtAV3.Text) + (TxtAY1.Text * TxtAV2.Text * TxtAW3.Text))
                m44 = -((TxtAV3.Text * TxtAW2.Text * TxtAX1.Text) + (TxtAW3.Text * TxtAX2.Text * TxtAV1.Text) + (TxtAX3.Text * TxtAV2.Text * TxtAW1.Text)) + ((TxtAV1.Text * TxtAW2.Text * TxtAX3.Text) + (TxtAW1.Text * TxtAX2.Text * TxtAV3.Text) + (TxtAX1.Text * TxtAV2.Text * TxtAW3.Text))
                Label65.Text = m44
            End If
        ElseIf RadioButton3.Checked = True Then
            If (TxtAV1.Text = 0 And TxtAW1.Text = 0 And TxtAX1.Text = 0 And TxtAY1.Text = 0) Or (TxtAV2.Text = 0 And TxtAW2.Text = 0 And TxtAX2.Text = 0 And TxtAY2.Text = 0) Or (TxtAV3.Text = 0 And TxtAW3.Text = 0 And TxtAX3.Text = 0 And TxtAY3.Text = 0) Or (TxtAV4.Text = 0 And TxtAW4.Text = 0 And TxtAX4.Text = 0 And TxtAY4.Text = 0) Then
                MessageBox.Show("No se puede resolver el sistema de ecuaciones porque no es cuadrado")
            Else

            End If
        End If
    End Sub
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If RadioButton1.Checked = True Then
            c11 = ((-1) ^ 2) * m11
            c12 = ((-1) ^ 3) * m12
            c13 = ((-1) ^ 4) * m13
            c21 = ((-1) ^ 3) * m21
            c22 = ((-1) ^ 4) * m22
            c23 = ((-1) ^ 5) * m23
            c31 = ((-1) ^ 4) * m31
            c32 = ((-1) ^ 5) * m32
            c33 = ((-1) ^ 6) * m33
            Label65.Text = c11 & "  " & c12 & "  " & c13 & vbCrLf & c21 & "  " & c22 & "  " & c23 & vbCrLf & c31 & "  " & c32 & "  " & c33
        ElseIf RadioButton2.Checked = True Then
            c11 = ((-1) ^ 2) * m11
            c12 = ((-1) ^ 3) * m12
            c13 = ((-1) ^ 4) * m13
            c14 = ((-1) ^ 5) * m14
            c21 = ((-1) ^ 3) * m21
            c22 = ((-1) ^ 4) * m22
            c23 = ((-1) ^ 5) * m23
            c24 = ((-1) ^ 6) * m24
            c31 = ((-1) ^ 4) * m31
            c32 = ((-1) ^ 5) * m32
            c33 = ((-1) ^ 6) * m33
            c34 = ((-1) ^ 7) * m34
            c41 = ((-1) ^ 5) * m41
            c42 = ((-1) ^ 6) * m42
            c43 = ((-1) ^ 7) * m43
            c44 = ((-1) ^ 8) * m44
            Label65.Text = c11 & "  " & c12 & "  " & c13 & "  " & c14 & vbCrLf & c21 & "  " & c22 & "  " & c23 & "  " & c24 & vbCrLf & c31 & "  " & c32 & "  " & c33 & "  " & c43 & vbCrLf & c41 & "  " & c42 & "  " & c43 & "  " & c44
        End If
    End Sub
    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        If RadioButton1.Checked = True Then
            ad11 = c11
            ad12 = c21
            ad13 = c31
            ad21 = c12
            ad22 = c22
            ad23 = c32
            ad31 = c13
            ad32 = c23
            ad33 = c33
            Label65.Text = ad11 & "" & ad12 & "" & ad13 & "" & ad14
        ElseIf RadioButton2.Checked = True Then
            ad11 = c11
            ad12 = c21
            ad13 = c31
            ad14 = c41
            ad21 = c12
            ad22 = c22
            ad23 = c32
            ad24 = c42
            ad31 = c13
            ad32 = c23
            ad33 = c33
            ad34 = c43
            ad41 = c14
            ad42 = c24
            ad43 = c34
            ad44 = c44
            Label65.Text = ad11 & "" & ad12 & "" & ad13 & "" & ad14
        End If
    End Sub
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        If RadioButton1.Checked = True Then
            A = ((TxtAV1.Text * c11) + (TxtAW1.Text * c12) + (TxtAX1.Text * c13))
            Label65.Text = A
        ElseIf RadioButton2.Checked = True Then
            A = ((TxtAV1.Text * c11) + (TxtAW1.Text * c12) + (TxtAX1.Text * c13) + (TxtAY1.Text * c14))
            Label65.Text = A
        End If
    End Sub
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        If RadioButton1.Checked = True Then
            b11 = ad11 / A
            b12 = ad12 / A
            b13 = ad13 / A
            b21 = ad21 / A
            b22 = ad22 / A
            b23 = ad23 / A
            b31 = ad31 / A
            b32 = ad32 / A
            b33 = ad33 / A
            x = ((b11 * TxtAC1.Text) + (b12 * TxtAC2.Text) + (b13 * TxtAC3.Text))
            y = ((b21 * TxtAC1.Text) + (b22 * TxtAC2.Text) + (b23 * TxtAC3.Text))
            z = ((b31 * TxtAC1.Text) + (b32 * TxtAC2.Text) + (b33 * TxtAC3.Text))
            Label65.Text = x & "  " & y & "  " & z
        ElseIf RadioButton2.Checked = True Then
            b11 = ad11 / A
            b12 = ad12 / A
            b13 = ad13 / A
            b14 = ad14 / A
            b21 = ad21 / A
            b22 = ad22 / A
            b23 = ad23 / A
            b24 = ad24 / A
            b31 = ad31 / A
            b32 = ad32 / A
            b33 = ad33 / A
            b34 = ad34 / A
            b41 = ad41 / A
            b42 = ad42 / A
            b43 = ad43 / A
            b44 = ad44 / A
            x = ((b11 * TxtAC1.Text) + (b12 * TxtAC2.Text) + (b13 * TxtAC3.Text) + (b14 * TxtAC4.Text))
            y = ((b21 * TxtAC1.Text) + (b22 * TxtAC2.Text) + (b23 * TxtAC3.Text) + (b24 * TxtAC4.Text))
            z = ((b31 * TxtAC1.Text) + (b32 * TxtAC2.Text) + (b33 * TxtAC3.Text) + (b34 * TxtAC4.Text))
            v = ((b41 * TxtAC1.Text) + (b42 * TxtAC2.Text) + (b43 * TxtAC3.Text) + (b44 * TxtAC4.Text))
            Label65.Text = x & "  " & y & "  " & z & "  " & v
        End If
    End Sub
End Class