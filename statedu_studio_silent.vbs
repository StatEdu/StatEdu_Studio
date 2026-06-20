Set shell = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")

appDir = fso.GetParentFolderName(WScript.ScriptFullName)
shell.CurrentDirectory = appDir
shell.Environment("PROCESS")("EASYFLOW_SILENT") = "1"
shell.Run """" & appDir & "\StatEdu_Studio.bat" & """", 0, False
