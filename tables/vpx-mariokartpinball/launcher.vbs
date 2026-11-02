Option Explicit
Randomize

'******************************************************
' 						OPTIONS
'******************************************************

Const EnableLeftMagnasave = 1		' 1 - on; 0 - off; if on then the right magnasave button let's you rotate all LUT's

'******************************************************
' 					TABLE INIT
'******************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim FSSMode:FSSMode = Table1.ShowFSS
Dim UseVPMDMD
UseVPMDMD = DesktopMode

LoadVPM "01120100", "GTS3.VBS", 3.10

Const cGameName = "andretti"

Dim VarRol,VarHidden
Dim UseVPMColoredDMD
UseVPMColoredDMD = 1
If Table1.ShowDT = true then 
	VarRol=0:VarHidden=1
	'Textbox2.visible = True
	'Textbox3.visible = True
Else 
	VarRol=1:VarHidden=0
	Textbox2.visible = False
	Textbox3.visible = False
End If

Sub InitVPM()
	PremierOptions = CInt("0" & LoadValue("Premier","Options")) : PremierSetOptions
	On Error Resume Next

	With Controller
		.GameName = cGameName 
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine		= "Mario Andretti - Premier/Gottlieb 1995" & vbNewLine & "VPX version by Steely and Rascal"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
         MusicOn
        .HandleMechanics = 0
        .Hidden = VarHidden
		.Games(cGameName).Settings.Value("sound") = 1
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
	' Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = True
	vpmBallImage = "ballimage8"
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 7
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 8, 0, 0, 0, 0, 0, 0
        .InitKick BallRelease, 90, 10
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 1
    End With


End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

If B2SOn = True then VarRol=0

	textBox1.visible = 0
	On Error Resume Next
		dim testnormalmaps : testnormalmaps = LampPr.objectSpaceNormalMap
		If Err Then 
			textbox1.text = "Please use VPX 10.6 revision 3751 or higher"
			textbox1.visible = 1
		End IF

Const UseSolenoids 	= 2

Const UseLamps     	= True
Const UseSync		= True
Const UseGI			= False        'true 'Opto

Set LampCallback    = GetRef("UpdateMultipleLamps")

Const SSolenoidOn   = "SolOn"
Const SSolenoidOff  = "SolOff"
Const SFlipperOn    = "fx_FlipperUp"
Const SFlipperOff   = "fx_FlipperDown"
Const SCoin			= "Quarter"

Const swLCoin = 0 
Const swRCoin = 1
Const swCCoin = 2
Const swCoinShuttle = 3
Const swStartButton = 4
Const swTournament = 5
Const swFrontDoor = 6
Const swDrop1 = 7

'**********************************************************************************************************
'Mario Kart Music
'**********************************************************************************************************
Sub MusicOn
	
    Dim FileSystemObject, folder, r, ct, file, musicPath, myMusicFolder
    myMusicFolder = "MK" 
    Set FileSystemObject = CreateObject("Scripting.FileSystemObject")
    ' Table Manager stores this table and its music folder under /external/vpx-mario.
    musicPath = "/media/usb0/external/vpx-mario/music/"
    If Not FileSystemObject.FolderExists(musicPath & myMusicFolder) Then Exit Sub

	Set folder = FileSystemObject.GetFolder(musicPath & myMusicFolder)

	'*****************************************************************************************************************
	'NOTE- If you use a non-standard folder structure {IE not using "VisualPinball\Tables" and "VisualPinball\Music"} 
	'please enter in the direct path to music sub-folder on the "Set Folder=" below and comment out "Set Folder=" Line above.
	'*****************************************************************************************************************
	
	'Set custom path below
	'Set folder= FileSystemObject.GetFolder("C:\vPinball\VisualPinball\Music\MK")
	
    Randomize
    r = INT(folder.Files.Count * Rnd + 1)
    ct=1
    For Each file in folder.Files
        if ct = r Then 
            if (LCase(Right(file,4))) = ".mp3" Then ' can only play mp3 files
               PlayMusic Mid(file, Len(musicPath) + 1, 1000) 
            End If 
       End If
   ct = ct + 1
   Next
 End Sub
 
Sub Table1_MusicDone() 
    MusicOn
 End Sub

 
 Sub Table1_MusicDone()
    MusicOn
 End Sub

Set Lights(0) = Light0
Set Lights(1) = Light1
Set Lights(2) = Light2
Set Lights(3) = Light3
Set Lights(4) = Light4
Set Lights(5) = Light7
Set Lights(6) = Light8
Set Lights(7) = Light9

Set Lights(12) = Light12   
Set Lights(13) = Light13
Set Lights(14) = Light14
Set Lights(15) = Light15   
Set Lights(16) = Light16
Set Lights(17) = Light17

Set Lights(22) = Light22
Set Lights(23) = Light23
Set Lights(24) = Light24
Set Lights(25) = Light25
Set Lights(26) = Light26
Set Lights(27) = Light27

Set Lights(30) = Light30
Set Lights(31) = Light31
Set Lights(32) = Light32
Set Lights(33) = Light33
Set Lights(34) = Light34
Set Lights(35) = Light35
Set Lights(36) = Light36
Set Lights(37) = Light37

Set Lights(40) = Light40
Set Lights(41) = Light41
Set Lights(42) = Light42
Set Lights(43) = Light43
Set Lights(44) = Light44
Set Lights(45) = Light45
Set Lights(46) = Light46
Set Lights(47) = Light47

Set Lights(50) = Light50
Set Lights(51) = Light51
Set Lights(52) = Light52
Set Lights(53) = Light53
Set Lights(54) = Light54
Set Lights(55) = Light55
Set Lights(56) = Light56
Set Lights(57) = Light57

Set Lights(60) = Light60
Set Lights(61) = Light61
Set Lights(62) = Light62
Set Lights(63) = Light63
Set Lights(64) = Light64
Set Lights(65) = Light65
Set Lights(66) = Light66
Set Lights(67) = Light67

Set Lights(70) = Light70
Set Lights(71) = Light71
Set Lights(72) = Light72
Set Lights(73) = Light73
Set Lights(74) = Light74
Set Lights(75) = Light75
Set Lights(76) = Light76
Set Lights(77) = Light77

Set Lights(80) = Light80
Set Lights(81) = Light81
Set Lights(82) = Light82
Set Lights(84) = Light84
Set Lights(85) = Light85
Set Lights(86) = Light86
Set Lights(87) = Light87

Sub UpdateMultipleLamps
    Light5.State=Light4.State
    Light6.State=Light4.State
End Sub


'Flippers/Slings/Jets/Trough/Knocker/Plunger
'SolCallback(sLLFlipper)		= "SolFlipper LeftFlipper,Nothing,"
'SolCallback(sLLFlipper)	= "vpmSolSound ""fx_Flipperup"","
'SolCallback(sLRFlipper) 	= "SolFlipper RightFlipper,Nothing,"
'SolCallback(sLRFlipper) = "vpmSolSound ""fx_Flipperup"","

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

	Sub SolLFlipper(Enabled)
		If Enabled Then
		PlaySound SoundFX("fx_FlipperUp",DOFFlippers)
		LeftFlipper.RotateToEnd
     Else
		 PlaySound SoundFX("fx_FlipperDown",DOFFlippers)
		 LeftFlipper.RotateToStart
     End If
	End Sub

	Sub SolRFlipper(Enabled)
     If Enabled Then
		PlaySound SoundFX("fx_FlipperUp",DOFFlippers)
		 RightFlipper.RotateToEnd
     Else
		 PlaySound SoundFX("fx_FlipperDown",DOFFlippers)
		 RightFlipper.RotateToStart
     End If
	End Sub

SolCallback(1)  = "vpmSolSound SoundFX(""Jet2"",DOFContactors),"
SolCallback(2)  = "vpmSolSound SoundFX(""Jet2"",DOFContactors),"
SolCallback(3)  = "vpmSolSound SoundFX(""Jet2"",DOFContactors),"

SolCallback(4)  = "vpmSolSound SoundFX(""LSling"",DOFContactors),"
SolCallback(5)  = "vpmSolSound SoundFX(""LSling"",DOFContactors),"

SolCallback(6)  = "bsLKicker.SolOut"
SolCallback(7)  = "bsRLowKicker.SolOut"
SolCallback(8)  = "vpmSolAutoPlungeS Plunger1, SoundFX(SSolenoidOn,DOFContactors), 2,"

SolCallback(9)  = "SolRampU" 'block between flippers
SolCallback(10) = "SolRampD"
'TEST
SolCallback(11) = "SOLGATETOP"'"Gate2.open="'"Topgate" plungergate
'SolCallback(11) = "Gate2.open="'"Topgate" plungergate
'SolCallback(12) = "gate3.open="'"botGate" plungergate
SolCallback(12) = "rightrampstop"

'SolCallback(13) = "vpmSolDiverter Flipper4,true,"'"botGatePull"   diverter
SolCallback(14) = "vpmSolDiverter Flipper6,true,"'"botGatehold"   diverter

'SolCallback(15) = "vpmSolDiverter Flipper7,true,"'"TopGatePull"   diverter
SolCallback(16) = "vpmSolDiverter Flipper5,true,"'"TopGatehold"   diverter

SolCallback(17) = "spinningflasher"
'SolCallback(17) =  "SpinningCar1" 
SolCallback(18) = "leftcarspin"    		
'SolCallback(18) =  "SpinningCar2"
SolCallback(19) = "rightcarspin"  
'SolCallback(19) =  SpinningCar3 

'SolCallback(18) = "vpmFlasher light28,"         'HomeRun

SolCallback(20) = "leftapronflasher"         'HomeRun
SolCallback(21) = "leftmiddleflasher"         '
SolCallback(22) = "lefttoplightstrip"         'G
SolCallback(23) = "righttoplightstrip"    
SolCallback(24) = "rightmiddleflasher"         '
SolCallback(25) = "rightapronflasher"         '

'"SolLightBoxRelay"
SolCallback(26) = "vpmSolSound SoundFX(""SolOn"",DOFContactors),"
'"TicketDispencer"
SolCallback(27) = "vpmSolSound SoundFX(""SolOn"",DOFContactors),"
SolCallback(28) = "bsTrough.SolOut"
SolCallback(29) = "bsTrough.SolIn" 
SolCallback(30) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(31) = "SolNugdeRelay"
SolCallback(32) = "GameOverRelay"

'by Dan Roth AKA Bubblehead

Dim TalkIndex,TalkArray,TalkWallMF, TalkWallMR
TalkIndex = 0
TalkArray = Array(560,75,75,75,75,200,125,150,150,150,140,40,150,135,150,40,40,150,150,40,40,40,150,40,40)'',150,40,40,40,150,440,40,40,150,150,40,40,150,150,40,40,150,150,440,60,60,150,80,100,60,60,60,60,60,850,500
Set TalkWallMF = W1
Set TalkWallMR = W2


Sub rightrampstop(Enabled)
	If Enabled Then Wall43.IsDropped = True Else Wall43.IsDropped = False
End Sub

' Spinner commands from VPM -  these three subs and their switches (triggers5, 13 & 14) still need to be fine tuned for realistic play. dated: 2/17/19
Sub spinningflasher(Enabled)
	Flasher1.Visible = Enabled
	Flasher4.Visible = Enabled
	If Enabled then
		If Primitive1.RotY <=270 then sVel = -1.75 + sVel Else sVel = 1.75 + sVel
	End If
End Sub

Sub leftcarspin(Enabled)
	Flasher1.Visible = Enabled
	Flasher4.Visible = Enabled
	If Enabled then
		If Primitive1.RotY <=30 then sVel = -1.75 + sVel Else sVel = 1.75 + sVel
	End If
End Sub

Sub rightcarspin(Enabled)
	Flasher1.Visible = Enabled
	Flasher4.Visible = Enabled
	If Enabled then
		If Primitive1.RotY <=150 then sVel = -1.75 + sVel Else sVel = 1.75 + sVel	
	End If
End Sub

Sub rightapronflasher(Enabled)
	Flasher2.Visible = Enabled
	Flasher3.Visible = Enabled
End Sub

Sub leftapronflasher(Enabled)
	Flasher5.Visible = Enabled
	Flasher6.Visible = Enabled
End Sub

Sub leftmiddleflasher(Enabled)
	Flasher7.Visible = Enabled
	Flasher8.Visible = Enabled
End Sub

Sub rightmiddleflasher(Enabled)
	Flasher9.Visible = Enabled
	Flasher10.Visible = Enabled
End Sub

Sub lefttoplightstrip(Enabled)
	Flasher11.Visible = Enabled
	Flasher12.Visible = Enabled
	Flasher13.Visible = Enabled
	Flasher14.Visible = Enabled
	Flasher15.Visible = Enabled
	Flasher16.Visible = Enabled
End Sub

Sub righttoplightstrip(Enabled)
	Flasher17.Visible = Enabled
	Flasher18.Visible = Enabled
	Flasher19.Visible = Enabled
	Flasher20.Visible = Enabled
	Flasher21.Visible = Enabled
	Flasher22.Visible = Enabled
End Sub

Sub Timer1_Timer()
	Timer1.Interval = TalkArray(TalkIndex)
	Select Case TalkIndex Mod 2
		Case 0
			W1.IsDropped = True
			W2.IsDropped = False
	Case 1
			W1.IsDropped = False
			W2.IsDropped = True
	End Select
	TalkIndex = TalkIndex + 1
	If TalkIndex > 20 then
		Timer1.Enabled = False
		W1.IsDropped = False
		W2.IsDropped = True
		
		Starttable1
	End If
	'If TalkIndex = 2 then PlaySound "?"
	If TalkIndex = 1 then
		W1.IsDropped = True
		W2.IsDropped = False

		Set TalkWallMF = W1
		Set TalkWallMR = W2
    
		W1.IsDropped = False
	End If
End Sub

'init table
Dim bsTrough,bsRLowKicker,bsLKicker
Dim cbCaptive
'Dim mGlove
       
' ----------Spinner Ball Variables ----------
Dim SpBall(1), sCntrX, sCntrY, Pi, sDegs, sRad, sVel, BallnPlay, SpinBall, RotAdj, PiFilln, bipJump

Sub Table1_Init()
LoadLUT
Timer1.Enabled = True

' -----------------Spinner Primative w/Balls ------- by Steely ----------- code starts here
'Utilize the physics of balls to create collision for a spinning toy

	Pi = Round(4*Atn(1),6)		'3.14159
	sCntrX = Primitive1.x		'xy center of spinner
	sCntrY = Primitive1.y
	sRad = sCntrX - 104			'radius of spinner, distance from spinner center to spinner post center
	sVel = 0							'spinner velocity set to zero to start
	bipJump = 0
	'Create 2 balls to act as spinner posts, replaces non-working rotational primitive collision
	Set SpBall(0) = Kicker4.CreateSizedBall(40)	'A smaller size ball would produce a "null error" at startup, why? Fix was added to kicker1_Hit
	SpBall(0).x = 90								'place ball at post xy locations
 	SpBall(0).y = 777
	SpBall(0).mass = .5
	SpBall(0).color = RGB(255,0,0)			'used for troubleshooting only
	SpBall(0).Visible = False					'hide balls
	Kicker4.Kick 0,0,0								'free up kicker

	Set SpBall(1) = Kicker4.CreateSizedBall(40)
	SpBall(1).x = 770
 	SpBall(1).y = 877
	SpBall(1).mass = .5
	SpBall(1).Visible = False
	Kicker4.Kick 0,0,0
End Sub

Sub Kicker4_Timer	'This timer runs constantly to hold balls in place

	SpBall(0).vely = -0.1		'Immobilize the spinner balls, neg Y velocity offset reflects the timer interval and table slope
 	SpBall(1).vely = -0.1
 	SpBall(0).velx = 0		
 	SpBall(1).velx = 0
 	'SpBall(0).z = 0
 	'SpBall(1).z = 0
	sDegs = Primitive1.RotY 										'this should be .RotZ, VP bug
	SpBall(0).x = sRad*cos(sDegs*(PI/180)) + sCntrX	'Place spinner balls to follow 3D primitive1, draw a circle
	SpBall(0).y = sRad*sin(sDegs*(PI/180)) + sCntrY
	SpBall(1).y = sCntrY - (SpBall(0).y - sCntrY)			'Reverse clone ball(0) movement for ball(1)
	SpBall(1).x = sCntrX - (SpBall(0).x - sCntrX)
																				'NOTE: a pos sVel spins counterclockwise, a neg value spins clockwise
	If sVel > 6 Then  sVel = 6 End If							'Limit the spin velocity
	If sVel < -6 Then  sVel = -6 End If
	If sVel > .005 Then 												'Slow the spin down
		sVel = sVel - .005
	ElseIf sVel < -.005 Then 
		sVel = Round(sVel + .005, 3)
	Else 
		sVel = 0															'Stop the spin completely
	End If
	sVel = Round(sVel, 3)
	If Primitive1.RotY <= 0 Then Primitive1.RotY = 360	'Set the rotation value for a positive reading of 0-360 degrees only.
	If Primitive1.RotY > 360 Then Primitive1.RotY = 1		'The normal output is both pos ,neg and goes beyond 360
	Primitive1.RotY = Primitive1.RotY + sVel					'Add velocity to spinner
	Primitive125.RotY = Primitive1.RotY							'Flasher light moves with spinner
	On Error Resume Next ' FIX: BallnPlay may have drained or been destroyed (Kicker9) since the collision
	If bipJump > 0 Then bipJump = bipJump -1:BallnPlay.velz = 0: Else bipJump = 0
	On Error Goto 0

End Sub

'A big THANKS goes out to Pinball Ken for the work he did on B2Bcollision and to the developers who added it to VP

Sub OnBallBallCollision(ball1, ball2, velocity)
'		Determine which ball (ball1 or ball2) is which, for the Spinner Ball and the Ball-in-play. Ball1 & ball2 IDs appear to be set at random.
	If ball1.ID = SpBall(0).ID then					'SpBall(0)'s table ID is 0
		Set SpinBall = SpBall(0)'or ball1			'Set spinner ball to = ball1
		Set BallnPlay = ball2							'Set ball-in-play to = ball2
		RotAdj = abs(Primitive1.RotY-360)		'* Take a sample of the primitive angle and adjust it's reading for calculations *
	ElseIf ball1.ID = SpBall(1).ID then				'SpBall(1)'s table ID is 1
		Set SpinBall = SpBall(1)
		Set BallnPlay = ball2
		RotAdj = abs(Primitive1.RotY-180)		'*
		If abs(Primitive1.RotY-360) < 180 Then RotAdj = abs(Primitive1.RotY-360) + 180		'*
	ElseIf ball2.ID = SpBall(0).ID then
		Set SpinBall = SpBall(0)
		Set BallnPlay = ball1
		RotAdj = abs(Primitive1.RotY-360)		'*
	ElseIf ball2.ID = SpBall(1).ID then
		Set SpinBall = SpBall(1)
		Set BallnPlay = ball1
		RotAdj = abs(Primitive1.RotY-180)		'*
		If abs(Primitive1.RotY-360) < 180 Then RotAdj = abs(Primitive1.RotY-360) + 180		'*
	Else
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(Ball1), 0, Pitch(Ball1), 0, 0, AudioFade(Ball1)
		Exit Sub	'Incase of multi-ball or any other non spinner ball collision, exit sub
	End If
	bipJump = 2	'Ball-in-play tends to jump off of smaller spinner ball with a strong hit, placed BallnPlay.velz = 0 in timer above

'NOTES to self: VBScript uses radians (vs degrees) to calculate trigonometry funtions (atn,tan,cos,sin...) So convert  " Radians = Degrees x Pi/180 "
'		Y coordinates on a VP table are pos/neg reversed compared to the mathematical unit circle & grid formulas, adjust accordingly, else you'll loose your mind.

'		PiFilln accounts for pos/neg values in the collision to provide a proper pos/neg spin velocity
	If SpinBall.X < BallnPlay.X Then PiFilln = Pi Else PiFilln = 0
	If SpinBall.X = BallnPlay.X Then Exit Sub ' FIX: avoid division by zero when balls are stacked on top of each other
		
'		So basically..... the new spin velocity = old spin velocity +- (spinner angle +- ball collision angle) * collision velocity
	sVel = sVel + sin((RotAdj * Pi/180) - atn(((SpinBall.Y - BallnPlay.Y) * -1)/(SpinBall.X - BallnPlay.X)) + PiFilln) * velocity/16

End Sub
'-------- End Spinner Ball code -------------

Sub StartTable1()
	InitVPM
	
'	vpmCreateEvents Gloves 
	
	' Nudging
	vpmNudge.TiltSwitch = 151
	vpmNudge.Sensitivity = 6
	
	'Place 4 th ball in the outhole
	Drain.CreateBall
	Drain.Kick 0,3	
	
'	InitAngles
'	InitLamp

	Captive1.CreateBall
	Captive1.Kick 180,1
	Captive1.Enabled = False
'	Kicker4.Kick 180,1	
	
	' BallStack
	Set bsTrough = New cvpmBallStack
	bsTrough.InitSw 25,0,0,15,0,0,0,0
	bsTrough.InitKick BallRelease, 90, 5 
	bsTrough.InitEntrySnd "SOLON", "SolOn"
	bsTrough.InitExitSnd SoundFX("BallRel",DOFContactors), SoundFX("SolOn",DOFContactors)
   bsTrough.Balls = 3
	bsTrough.BallImage="ballimage8"
 
	set bsRLowKicker = new cvpmBallStack
	bsRLowKicker.InitSw 0,91,0,0,0,0,0,0
 	bsRLowKicker.InitKick Kicker3,225,15
	bsRLowKicker.InitEntrySnd "Solenoid", "Solenoid"
	bsRLowKicker.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
	bsRLowKicker.Balls = 0	
	bsRLowKicker.KICKBallS = 4	
	bsRLowKicker.BallImage = "ballimage8"
	
	set bsLKicker = new cvpmBallStack
 	bsLKicker.InitSaucer kicker2,24,-135,14
	bsLKicker.InitExitSnd SoundFX("BallRel",DOFContactors), SoundFX("Solenoid",DOFContactors)

	Wall41.IsDropped = True
	LeftBlock.isdropped=true
	plunger1.pullback 
        
End Sub

Sub SolNugdeRelay(enabled)
	if enabled then
		playsound "SolOn"
		'Bumper1.disabled = 1
		'Bumper2.disabled = 1
		'Bumper3.disabled = 1
		LeftSlingshot.disabled = 1
		RightSlingshot.disabled = 1
	else
		'Bumper1.disabled = 0
		'Bumper2.disabled = 0
		'Bumper3.disabled = 0
		LeftSlingshot.disabled = 0
		RightSlingshot.disabled = 0
	end if 
end sub


Sub GameOverRelay(enabled)
	if enabled then
		playsound "SolOn"
		'Bumper1.disabled = 0
		'Bumper2.disabled = 0
		'Bumper3.disabled = 0
		LeftSlingshot.disabled = 0
		RightSlingshot.disabled = 0
		'lighttilt.state = 1
	else
		'Bumper1.disabled = 1
		'Bumper2.disabled = 1
		'Bumper3.disabled = 1
		LeftSlingshot.disabled = 1
		RightSlingshot.disabled = 1
		'lighttilt.state = 0
	end if 
end sub


'TEST
Sub SOLGATETOP(enabled)
	if enabled then
		playsound SoundFX("FLAPclos",DOFContactors)
		'GATE2.OPEN = 0
		Wall41.IsDropped = False
			else
		'GATE2.OPEN = 1
		Wall41.IsDropped = True
	end if 
end sub

'Const StartButton = 2  
'ExtraKeyHelp = KeyName(StartButton) & vbTab & "StartButton"

'//////////////////////////////////////////////////////////////////////
'// LUT
'//////////////////////////////////////////////////////////////////////

'//////////////---- LUT (Colour Look Up Table) ----//////////////
'0 = Fleep Natural Dark 1
'1 = Fleep Natural Dark 2
'2 = Fleep Warm Dark
'3 = Fleep Warm Bright
'4 = Fleep Warm Vivid Soft
'5 = Fleep Warm Vivid Hard
'6 = Skitso Natural and Balanced
'7 = Skitso Natural High Contrast
'8 = 3rdaxis Referenced THX Standard
'9 = CalleV Punchy Brightness and Contrast
'10 = HauntFreaks Desaturated
'11 = Tomate Washed Out 
'12 = VPW Original 1 to 1
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book

Dim LUTset, DisableLUTSelector, LutToggleSound, bLutActive
LutToggleSound = True
LoadLUT
'LUTset = 16			' Override saved LUT for debug
SetLUT
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

Sub Table1_exit:SaveLUT:Controller.Stop:End Sub

Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	Select Case LUTSet
		Case 0: LUTBox.text = "Fleep Natural Dark 1"
		Case 1: LUTBox.text = "Fleep Natural Dark 2"
		Case 2: LUTBox.text = "Fleep Warm Dark"
		Case 3: LUTBox.text = "Fleep Warm Bright"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 6: LUTBox.text = "Skitso Natural and Balanced"
		Case 7: LUTBox.text = "Skitso Natural High Contrast"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "HauntFreaks Desaturated"
  		Case 11: LUTBox.text = "Tomate washed out"
        Case 12: LUTBox.text = "VPW original 1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"
        Case 15: LUTBox.text = "B&W Comic Book"
		Case 16: LUTBox.text = "Skitso New ColorLut"
	End Select
	LUTBox.TimerEnabled = 1
End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "MarioKartLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
    bLutActive = False
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=0
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "MarioKartLUT.txt") then
		LUTset=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "MarioKartLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=0
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

'******************************************************
' 					KEYS
'******************************************************

Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftFlipperKey then Controller.Switch(82) = 1
	If keycode = RightFlipperkey then Controller.Switch(83) = 1
	If vpmKeyDown(keycode) Then Exit Sub

'LUT controls
If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
     MusicOn
            If bLutActive Then
                    if DisableLUTSelector = 0 then
                LUTSet = LUTSet  - 1
                if LutSet < 0 then LUTSet = 16
                SetLUT
                ShowLUT
            End If
        End If
        End If

	'If keycode = LeftMagnaSave then 
	'	If globalGI <46 Then 
	'		globalGI = 46 
	'	Else
	'		globalGI = 10
	'	end if
	'End if
	If keycode = PlungerKey Then
		Plunger.Pullback
	End if
'--------- Make Spinner Balls Visable or Not -----------
	If keycode = 36 Then	'J
		If SpBall(0).Visible = False Then
			SpBall(0).Visible = True:SpBall(1).Visible = True
		Else
			SpBall(0).Visible = False:	SpBall(1).Visible = False
		End If
	End If
'--------- Make Spinner Balls Visable or Not -----------

	if keycode = 19 then Show_Information	'R
	
End Sub

Sub Table1_KeyUp(ByVal keycode)
'LUT controls
If keycode = LeftMagnaSave Then bLutActive = False
    If keycode = LeftFlipperKey then Controller.Switch(82) = 0
	If keycode = RightFlipperkey then Controller.Switch(83) = 0
	if vpmKeyUp(keycode) Then Exit Sub
	if keycode = PlungerKey Then
	Plunger.Fire	
End if
End Sub

Sub Show_Information()
	Dim info, rboxstyle
	rboxstyle = vbInformation
	info = "GAME INFORMATION"   & vbNewLine
	info = info & "" & vbNewLine
	info = info & "INGAME OPTIONS" & vbNewLine
	info = info & "PRESS F6 FOR PLAYFIELD ADJUSTMENTS" & vbNewLine
	info = info & "" & vbNewLine	
	info = info & "FACTORY RESET" & vbNewLine
	info = info & "1) PRESS 7 TO ENTER GAME MENU" & vbNewLine
	info = info & "2) SELECT GAME ADJUSTMENTS WITH LEFT FLIPPER" & vbNewLine
	info = info & "3) PRESS 7" & vbNewLine
	info = info & "4) PRESS 1 TO SELECT THE FACTORY RESET"& vbNewLine
	info = info & "5) PRESS 1 AGAIN TO ACTIVED" & vbNewLine
	info = info & "6) EXIT GAME AND LOAD AGAIN"
	MsgBox	info, rboxstyle, ""
end sub

Dim PremierOptions

Private Sub PremierShowDips
	If Not IsObject(vpmDips) Then ' First time
		Set vpmDips = New cvpmDips
		With vpmDips
			.AddForm 220, 240, "Game options"
			.AddFrame 0, 0, 80, "Country", &Hf0,_
				Array("USA", &H00, "USA", &Hf0, "European", &Hd0,_
				      "Export", &Ha0, "Export Alt", &H80, "France", &Hb0,_
				      "France 1", &H10, "France 2", &H30, "France 3", &H90,_
			    	  "Germany", &H20, "Spain",     &He0, "UK", &Hc0)
			.AddFrameExtra 90, 0, 140, "Not used", 0, array("Not used1",0,"Not used2",0)
			.AddFrameExtra 90,120,140,"conservative liberal",0,_
			    Array("easy Rightpost", &H01)
		End With
	End If
	PremierOptions = vpmDips.ViewDipsExtra(PremierOptions)
	PremierSetOptions :	SaveValue "Premier","Options",PremierOptions
End Sub

Set vpmShowDips = GetRef("PremierShowDips")
Sub PremierSetOptions
	if (PremierOptions And &H01) = 1 then
		OutPegR.TransZ=-25
		OutPegRubberR.TransY=25
		OutPegScrewR.TransZ=-25
		OutPegL.TransZ=-25
		OutPegRubberL.TransY=25
	else
		OutPegR.TransZ=0
		OutPegRubberR.TransY=0
		OutPegScrewR.TransZ=0
		OutPegL.TransZ=0
		OutPegRubberL.TransZ=0
	end if 
End Sub


Sub Drain_Hit()
    bsTrough.Addball me
End Sub

'addball to stacks
Sub Kicker3_Hit()
	bsRLowKicker.AddBall Me 
End Sub

Sub Kicker1_Hit()
	If Activeball.ID < 2 Then		'The spinner balls pass over this kicker producing a hit. Even though the ball still moves with the spinner...
		Kicker1.Kick 0,0,0				'we need to clear the kicker with a kick, so it will function with a game ball
		SpBall(0).Radius = 12 		'For some unknowen reason a small ball size when the ball is created, at table start, would produce a null error, but it works here.
		SpBall(1).Radius = 12		'Reduce spinner ball size to spinner post size
		Exit Sub
	End If
   vpmTimer.PulseSwitch(110),0,""	
	bsRLowKicker.AddBall Me 
End Sub

Sub Kicker2_Hit()	
	bsLKicker.AddBall 0 
End Sub


'-------------------------
Sub LeftSlingShot_SlingShot()  : vpmTimer.PulseSwitch 13,0,0 : End Sub
Sub RightSlingShot_SlingShot() : vpmTimer.PulseSwitch 14,0,0 : End Sub

Sub Bumper1_Hit() : vpmTimer.PulseSwitch 11,0,0 : End Sub
Sub Bumper2_Hit() : vpmTimer.PulseSwitch 10,0,0 : End Sub
Sub Bumper3_Hit() : vpmTimer.PulseSwitch 12,0,0 : End Sub

'outlanes klaar
Sub RightOutlane_Hit()	: Controller.Switch(93) = 1 : End Sub
Sub RightOutlane_UnHit(): Controller.Switch(93) = 0 : End Sub
Sub LeftOutlane_Hit()	: Controller.Switch(92) = 1 : End Sub
Sub LeftOutlane_UnHit()	: Controller.Switch(92) = 0 : End Sub
'IN LANES KLAAR
Sub LeftInlane_Hit()	: Controller.Switch(102) = 1 : End Sub
Sub LeftInlane_UnHit()	: Controller.Switch(102) = 0 : End Sub
Sub RightInlane_Hit()	: Controller.Switch(103) = 1 : End Sub
Sub RightInlane_UnHit()	: Controller.Switch(103) = 0 : End Sub

Sub Trigger2_Hit()      : Controller.Switch(23) = 1 : End Sub
Sub Trigger2_UnHit()    : Controller.Switch(23) = 0 : End Sub

'Spinner macro switches
Sub Trigger5_Hit()  : If ActiveBall.ID = 0 Then :Controller.Switch(20) = 1:End If:end sub	'Use SpBall(0).ID to trig spinner switches
Sub Trigger13_Hit() : If ActiveBall.ID = 0 Then :Controller.Switch(21) = 1:End If:end sub
Sub Trigger14_Hit() : If ActiveBall.ID = 0 Then :Controller.Switch(22) = 1:End If:end sub
Sub Trigger5_UnHit()  : If ActiveBall.ID = 0 Then :Controller.Switch(20) = 0:End If:end sub
Sub Trigger13_UnHit() : If ActiveBall.ID = 0 Then :Controller.Switch(21) = 0:End If:end sub
Sub Trigger14_UnHit() : If ActiveBall.ID = 0 Then :Controller.Switch(22) = 0:End If:end sub

'hairpin opto switches
Sub Trigger3_Hit()		:vpmTimer.PulseSwitch(101),0,"" : End Sub	'vpmTimer.PulseSwitch(21),0,"":end sub': Controller.Switch(101) = 1 : 
Sub Trigger4_Hit()		:vpmTimer.PulseSwitch(111),0,"" :	End Sub	'vpmTimer.PulseSwitch(22),0,"":end sub' Controller.Switch(111) = 1 : 

'left side rollover
Sub Trigger1_Hit()		: Controller.Switch(112) = 1 : End Sub
Sub Trigger1_UnHit()	: Controller.Switch(112) = 0 : End Sub

'right orbit
Sub Trigger6_Hit()		: Controller.Switch(81) = 1 : End Sub
Sub Trigger6_UnHit()	: Controller.Switch(81) = 0 : End Sub

'skillshot
Sub Wall22_Hit(): vpmTimer.PulseSwitch(84),0, "" : End Sub
Sub Trigger7_Hit()		: Controller.Switch(114) = 1 : End Sub   '/
Sub Trigger7_UnHit()	: Controller.Switch(114) = 0 : End Sub   '/
Sub Trigger8_Hit()		: Controller.Switch(104) = 1 : End Sub   '/
Sub Trigger8_UnHit()	: Controller.Switch(104) = 0 : End Sub   '/
Sub Trigger9_Hit()		: Controller.Switch(94) = 1 : End Sub
Sub Trigger9_UnHit()	: Controller.Switch(94) = 0 : End Sub
'leftramp
Sub Trigger10_Hit()		: Controller.Switch(80) = 1 : End Sub
Sub Trigger10_UnHit()	: Controller.Switch(80) = 0 : End Sub
Sub Trigger11_Hit()		: Controller.Switch(90) = 1 : End Sub
Sub Trigger11_UnHit()	: Controller.Switch(90) = 0 : End Sub
'rightramp
Sub Trigger12_Hit()		: Controller.Switch(100) = 1 : End Sub   '/
Sub Trigger12_UnHit()	: Controller.Switch(100) = 0 : End Sub   '/

'standup's
'-----------------------
'center 2X
Wall79.IsDropped=True
Sub Wall15_Hit(): vpmTimer.PulseSwitch(113),0, "":Wall15.IsDropped=True:Wall79.IsDropped=False:Wall79.TimerEnabled=True :End Sub  'Tachometer
Sub Wall79_Timer():Wall79.IsDropped=True:Wall15.IsDropped=False:Wall79.TimerEnabled=False:End Sub

Wall80.IsDropped=True
Sub Wall138_Hit():vpmTimer.PulseSwitch(113),0, "":Wall138.IsDropped=True:Wall80.IsDropped=False:Wall80.TimerEnabled=True :End Sub  'Tachometer
Sub Wall80_Timer():Wall80.IsDropped=True:Wall138.IsDropped=False:Wall80.TimerEnabled=False:End Sub

'rightside
Wall81.IsDropped=True
Sub Wall16_Hit(): vpmTimer.PulseSwitch(115),0, "":Wall16.IsDropped=True:Wall81.IsDropped=False:Wall81.TimerEnabled=True :End Sub  'Upperhole
Sub Wall81_Timer():Wall81.IsDropped=True:Wall16.IsDropped=False:Wall81.TimerEnabled=False:End Sub

'RightBottom
Wall82.IsDropped=True 
Sub Wall17_Hit(): vpmTimer.PulseSwitch(95),0, "":Wall17.IsDropped=True:Wall82.IsDropped=False:Wall82.TimerEnabled=True :End Sub 
Sub Wall82_Timer():Wall82.IsDropped=True:Wall17.IsDropped=False:Wall82.TimerEnabled=False:End Sub  

'leftside
Wall84.IsDropped=True
Sub Wall18_Hit(): vpmTimer.PulseSwitch(105),0, "":Wall18.IsDropped=True:Wall84.IsDropped=False:Wall84.TimerEnabled=True :End Sub   'Turbo
Sub Wall84_Timer():Wall84.IsDropped=True:Wall18.IsDropped=False:Wall84.TimerEnabled=False:End Sub 

Wall85.IsDropped=True
Sub Wall139_Hit():vpmTimer.PulseSwitch(105),0, "":Wall139.IsDropped=True:Wall85.IsDropped=False:Wall85.TimerEnabled=True :End Sub   'Turbo
Sub Wall85_Timer():Wall85.IsDropped=True:Wall139.IsDropped=False:Wall85.TimerEnabled=False:End Sub 

Wall83.IsDropped=True
Sub Wall19_Hit(): vpmTimer.PulseSwitch(85),0, "":Wall19.IsDropped=True:Wall83.IsDropped=False:Wall83.TimerEnabled=True : End Sub   'LeftBottom
Sub Wall83_Timer():Wall83.IsDropped=True:Wall19.IsDropped=False:Wall83.TimerEnabled=False:End Sub 

Sub	Kicker9_Hit()
    Kicker9.Destroyball
    Kicker11.CreateBall
	Kicker11.Kick 120,3
End Sub	

Sub SolRampD(Enabled)
If (Enabled) Then
LeftBlock.IsDropped = True
playsound SoundFX("flapclos",DOFContactors)
End If
End Sub

Sub SolRampU(Enabled)
If Enabled Then
LeftBlock.IsDropped = False
playsound SoundFX("flapclos",DOFContactors)
End If
End Sub


Sub flippertimer_Timer()
	LeftFlipperP.ObjRotZ = LeftFlipper.CurrentAngle-90
	RightFlipperP.ObjRotZ = RightFlipper.CurrentAngle-90
End Sub

Dim n
Dim loopvar
Dim Digits(32)
If Table1.ShowDT = true then
	For Each loopvar in dtdisplay
		loopvar.visible=True
	Next
	For Each loopvar in fsdisplay
		loopvar.visible=False
	Next
	Digits(0)=Array(a00,a01,a02,a03,a04,a05,a06,n,a08)
	Digits(1)=Array(a10,a11,a12,a13,a14,a15,a16,n,a18)
	Digits(2)=Array(a20,a21,a22,a23,a24,a25,a26,n,a28)
	Digits(3)=Array(a30,a31,a32,a33,a34,a35,a36,n,a38)
	Digits(4)=Array(a40,a41,a42,a43,a44,a45,a46,n,a48)
	Digits(5)=Array(a50,a51,a52,a53,a54,a55,a56,n,a58)
Else
	For Each loopvar in dtdisplay
		loopvar.visible=False
	Next
	For Each loopvar in fsdisplay
		loopvar.visible=True
	Next
	Digits(0)=Array(b00,b01,b02,b03,b04,b05,b06,n,b08)
	Digits(1)=Array(b10,b11,b12,b13,b14,b15,b16,n,b18)
	Digits(2)=Array(b20,b21,b22,b23,b24,b25,b26,n,b28)
	Digits(3)=Array(b30,b31,b32,b33,b34,b35,b36,n,b38)
	Digits(4)=Array(b40,b41,b42,b43,b44,b45,b46,n,b48)
	Digits(5)=Array(b50,b51,b52,b53,b54,b55,b56,n,b58)
End If

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else

			end if
		next
	end if	
End Sub

Sub flippertimer_Timer()
	LeftFlipperP.ObjRotZ = LeftFlipper.CurrentAngle-90
	RightFlipperP.ObjRotZ = RightFlipper.CurrentAngle-90
End Sub

Dim ball
'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the AudioPan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'//////////////////////////////////////////////////////////////////////
'// JP VPX ROLLING SOUNDS 
'//////////////////////////////////////////////////////////////////////

Const tnob = 12 ' FIX: was 5. GetBalls also counts the 2 hidden spinner balls + captive ball, so 4-ball multiball = 7 balls and overflowed rolling()
ReDim rolling(tnob)
InitRolling

Sub InitRolling
Dim i
For i = 0 to tnob
rolling(i) = False
Next
End Sub

Sub ballrollingtimer_Timer()
	Dim BOT, b
	BOT = GetBalls
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("fx_ballrolling" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

       ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If b > tnob Then Exit For ' FIX: never index past rolling()
        If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
			if BOT(b).z < 30 Then ' Ball on playfield
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.6, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
				End If
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'Additional Table Sounds
Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub
