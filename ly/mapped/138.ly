\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #138
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  \repeat volta 3 {
    \oneVoice
    b8 |
    e e g \tieDashed e~ e g | \tieSolid
    e4 g8 e4 fis16 fis |
    
    g8[ fis] e d[ d] c8 |
    b4 b'8\rest b4\rest \bar"" b,16[ b] |
    e8[ e] g8 e8[ e] g16[ g] |
    e4 g8 e4 fis16[ fis] |
    g8[ fis] g a4 a8 |
    
    \voiceOne
    b4.~ b8 b\rest \bar"" b16[ b] |
    b8[ b] c8 b[ b] c8 |
    b[ b] c b4 c16[ c] |
    b8[ b] a8 g[ a] b |
    a4. b4\rest \bar"" d,16 d |
    
    g4 a8 g[ g] a |
    g[ g] a g[ g] a16[ a] |
    g8[ g] e8 c4 b8 |
    ais2.~ |
    ais4 ais8 ais4 ais8 |
    
    b4.~ b8\fermata b'\rest \bar"" b,16[ b] \bar "||"
    
    \break
    \key e \major
    gis'4 b,8 cis4 e8 |
    gis8[ gis] b,8 cis4 e16 e |
    gis8[ a] b b4 gis8 |
    fis4.~ fis8 b\rest \bar"" b16[ b] |
    
    %page2/143 melody
    b8[ gis] b b b\rest gis |
    gis[ e] gis gis b\rest e, |
    e[ cis] e e4 dis8 |
    e4.~ e8 b'\rest \bar"" b |
    e8[ e] dis8 cis4 b8 |
    
    cis4 b8 a4 gis8 |
    fis8[ fis] e8 dis4 e8 |
    fis4.~ fis8 b\rest \bar"" b |
    b8[ gis] b b4 gis8 |
    gis[ e] gis gis4 e8 |
    
    e[ cis] e e4 dis8 |
  }
  \alternative {
    {
       \partial 8*5
      e4.~ e8 b'8\rest
    }
    {
      e,8 b'8\rest e,8 e8. fis16 e8 |
      
      e8. fis16 e8 e8. fis16 e8 |
      e'4.~ e4 e,8 |
      e4. e |
      b'2.~ |
      b \bar"|."
    }
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The crim -- in -- al \set ignoreMelismata = ##t cried, as he dropp’d him down, \unset ignoreMelismata
  In a state of wild a -- larm—
  \set ignoreMelismata = ##t With a \unset ignoreMelismata fright -- ful, fran -- tic, __ fear -- ful frown I __ bared my big right arm.
  I __ seiz’d him by __ his \set ignoreMelismata = ##t lit -- tle pig -- tail, \unset ignoreMelismata
  And on __ his knees fell he,
  As he squirm’d and \set ignoreMelismata = ##t strug -- gled
  And gur -- gled and gur -- gled, \unset ignoreMelismata 
  I __ drew my snick -- er snee, __
  my snick -- er snee! __
  
  Oh __ ne’er shall I
  For -- get __ the cry,
  Or the shriek that shriek -- ed he, __
  \set ignoreMelismata = ##t As I \unset ignoreMelismata gnash’d my teeth,
  When from its sheath
  I drew my snick -- er -- snee! __
  
  We know him well,
  He can -- not tell
  Un -- true or ground -- less tales.
  He al -- ways tries
  To ut -- ter lies,
  And ev -- ’ry time he fails.
  
  
  says! Ex -- act -- ly, ex -- act -- ly, ex -- act -- ly, ex -- act -- ly as he says! __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  He shiv -- er’d and \set ignoreMelismata = ##t shook as he gave the sign \unset ignoreMelismata
  For the stroke he \set ignoreMelismata = ##t did -- n’t de -- serve, \unset ignoreMelismata
  When \set ignoreMelismata = ##t all of a sud -- den his  __ _ eye met mine,
  And it \unset ignoreMelismata seem’d to brace his nerve,
  \set ignoreMelismata = ##t For he nod -- ded \unset ignoreMelismata his head and kiss’d his hand,
  \set ignoreMelismata = ##t And he whis -- tled an \unset ignoreMelismata air, __ did he,
  As the sa -- bre true __
  Cut clean -- ly through
  His \set ignoreMelismata = ##t cer -- vi -- cal \unset ignoreMelismata ver -- te -- bræ, __ his ver -- te -- bræ! __
  
  \set ignoreMelismata = ##t When a man’s a -- fraid
  A beau -- ti -- ful maid
  Is a \unset ignoreMelismata cheer -- ing sight to see; __
  \set ignoreMelismata = ##t And it’s \unset ignoreMelismata oh, __ I’m glad,
  That mo -- ment sad
  Was sooth’d by sight of me! __
  
  Her \set ignoreMelismata = ##t ter -- ri -- ble tale \unset ignoreMelismata 
  You can’t as -- sail,
  With truth it quite a -- grees;
  Her taste ex -- act
  For fault -- less fact
  A -- mounts to a dis -- ease.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t 
  Now tho’ you’d have said __ _ that head was dead
  (For its own -- _ er dead _ was he),
  It _ stood on its neck _ with a smile well bred,
  And _ bow’d _ three times to me! _
  It was none of your im -- pu -- dent off -- _ hand nods,
  But as hum -- _ ble as __ _ could be,
  For it clear -- ly knew __ _
  The def -- er -- ence due __ _
  To a man _ of ped -- i -- gree, __ _
  of ped -- i -- gree! __ _
  
  And it’s oh, I vow,
  This death -- _ ly bow
  Was a touch -- _ ing sight to see; __ _
  Though _ trunk -- _ less, yet
  It could -- n’t for -- get
  The def -- er -- ence due to me! __ _
  
  The haugh -- _ ty youth
  He speaks the truth
  When -- ev -- er he finds it pays, _
  And in _ this case
  It all _ took place
  Ex -- act -- _ ly as he
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat volta 3 {
    s8 |
    s2.*7 |
    s2 s8 b'16[ b] |
    
    g8[ g] g g[ g] g |
    g[ g] g g4 g16[ g] |
    g8[ g] g e4 e8 |
    fis4. s4 d16 d |
    
    e4 f8 e[ e] f8 |
    e[ e] f e[ e] f16[ f] |
    e8[ e] e c4 b8 |
    ais4.( g |
    gis4) gis8 g4 g8 |
    b4.~ b8 s b16[ b] |
    
    \key e \major
    e4 gis,8 a4 cis8 |
    e8[ e] gis,8 a4 cis16 cis |
    e8[ fis] gis gis4 e8 |
    dis4.~ dis8 s dis16[ dis] |
    
    e4 fis8 gis s bis, |
    cis[ cis] dis8 e s b? |
    cis[ cis] cis8 cis4 a8 |
    b4.~ b8 s8
    
    
    
    b'8 |
    e,[ e] e8 e4 e8 |
    e4 e8 e4 e8 |
    
    fis8[ fis] e dis4 e8 |
    dis4.~ dis8 s dis |
    e4 fis8 gis4 gis8 |
    cis,4 dis8 e4 b8 |
    e[ cis] e e4 dis8
  }
  \alternative {
    {
      e4.~ e8 s |
    }
    {
      b8 s e8 e8. fis16 e8 |
      e8. fis16 e8 e8. fis16 e8 |
      a4.~ a4 e8 |
      e4. e |
      gis2.~ |
      gis \bar"|."
    }
  }
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \repeat volta 3 {
    s8 |
    s2.*7 |
    s2 s8 b16[ b] |
    d8[ d] e d[ d] e |
    d[ d] e d4 e16[ e] |
    d8[ d] d cis4 cis8 |
    d4. s4 d16 d |
    
    c4 c8 c[ c] c |
    c[ c] c c[ c] c16[ c] |
    c8[ c] e c4 b8 |
    ais4.( e |
    f4) f8 e4 e8 |
    dis4.~ dis8 s b16[ b] |
    
    
    \key e \major
    b'4 e,8 e4 e8 |
    b'[ b] e,8 e4 e16 e |
    b'4 e,8 e4 gis8 |
    b4.~ b8 s b16[ b] |
    
    gis4 b8 b s gis |
    gis[ gis] fis8 e8 s e |
    e[ e] e8 a4 fis8 |
    gis4.~ gis8 s
    
    
    
    
    b8 |
    gis[ gis] gis8 gis4 gis16[ b] |
    e4 dis8 cis4 b8 |
    
    ais8[ ais] ais ais4 ais8 |
    b4.~ b8 s b |
    b4 a8 gis4 gis8 |
    gis4 fis8 e4 e8 |
    e4 e8 a4 a8
  }
  \alternative {
    {
      gis4.~ gis8 s |
    }
    {
      gis s e e8. fis16 e8
      e8. fis16 e8 e8. fis16 e8 |
      cis'4.~ cis4 cis8 |
      cis4. cis |
      e2.~ |
      e \bar"|."
    }
  }
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat volta 3 {
    \oneVoice
    b,8 |
    e e g \tieDashed e~ e g | \tieSolid
    e4 g8 e4 fis16 fis |
    
    g8[ fis] e d[ d] c8 |
    b4 d8\rest d4\rest \bar"" b16[ b] |
    e8[ e] g8 e8[ e] g16[ g] |
    e4 g8 e4 fis16[ fis] |
    g8[ fis] g a4 a8 |
    
    b4.~ b8 d,\rest
    \voiceTwo
    b'16[ b] |
    g8[ g] c g[ g] c |
    g[ g] c g4 c16[ c] |
    g8[ g] g g4 g8 |
    d4. d4\rest d16 d |
    
    c4 f8 c[ c] f |
    c[ c] f c[ c] f16[ f] |
    c8[ c] e c4 b8 |
    ais4.( cis |
    d4) d8 cis4 cis8 |
    b4.~ b8\fermata d\rest b16[ b] |
    
    
    \key e \major
    e4 e,8 a4 a8 |
    e[ e] e8 a4 a16 a |
    e4 e8 e4 e8 |
    b'4.~ b8 s b16[ b] |
    
    cis4 dis8 e d\rest gis, |
    ais[ ais] bis8 cis d\rest gis, |
    a[ a] gis8 fis4 b8 |
    e4.~ e8 d\rest
    
    
    
    
    
    
    b'8 |
    e,[ e] e8 e4 e8 |
    e4 e8 e4 e8 |
    
    cis[ cis] cis fis4 fis8 |
    b,4.~ b8 d\rest b |
    cis4 dis8 e4 gis,8 |
    ais4 bis8 cis4 gis8 |
    a4 gis8 fis4 b8
  }
  \alternative {
    {
      e,4.~ e8 s |
    }
    {
      e8 d'\rest e e8. fis16 e8 |
      e8. fis16 e8 e8. fis16 e8 |
      a4.~ a4 a8 |
      a4. a |
      e2.~ |
      e \bar"|."
    }
  }
  
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The criminal cried"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"Arthur Sullivan (1842–1900)"
  tagline = ""
}}
