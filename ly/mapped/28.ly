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
    #'((basic-distance . -1)
       (minimum-distance . -1)
       (padding . -3)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 0))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 70))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #28
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
  \tempo "Allegro vivace" 4. = 116
	\partial 2
  d'8 d4 e8 |
  d4-> a8 d4 e8 |
  d4 d,8 d4 e8 |
  d4-> a'8 a4 a8 |
  
  c4.->~ c~ |
  c4 b8\rest b4\rest\fermata d,8 |
  g4. e4( d8) g4. e4( d8) |
  g4.\< b |
  d4\!->  b8\mf d4 e8 |
  
  d4 a8 c4 e8 |
  d4 b8 g4 b8 |
  a4. e |
  a~ a8 b\rest d, |
  
  %page2/61
  g4. e4( d8) |
  \slurDashed g4. e4( d8) 
  \slurSolid g4.\< b |
  d4\f-> b8 b4 b8 |
  d4 cis8 d4 e8 |
  
  fis4-> d8 cis4 b8 |
  a4. e' |
  d d,4\mf d8 |
  \tieDashed c'4. c4~ c8 |
  \tieSolid c4. b4( a8) |
  
  b4. g4( e8) |
  \tieDashed d4. d4~ d8 |
  c'4. c4~ c8 |
  \tieSolid c4. b4 a8 |
  b4. g4( e8) |
  
  %page3/62
  d4. g4 g8 |
  \tieDashed fis4.\< b4~ b8 |
  \slurDashed d4. cis4( b8) |
  \slurSolid fis'4.\! cis4( e8) |
  \tieSolid d4. d |
  
  \slurDashed d\cresc a4\!( fis8) |
  \slurSolid e4.~ e4 fis8 |
  d4 d'8\ff d4 e8 |
  d4.~ d8\fermata d,4\mf \time 2/4 \break
  
  \tempo "Allegretto" 4 = 138
  g4-> g |
  g a8[ g] |
  fis([ a] d4->)~ |
  d d, |
  a'4->\< a |
  a b8[\! a] |
  g([ b] d4->)~ |
  d d |
  \once\override DynamicText #'self-alignment-X = #4
  e4.->\f c8 |
  
  %page4/63
  c4 e |
  e-> d |
  b4. b8 |
  b4-> a |
  a e |
  fis8[( g] a4)~ |
  a d,\mp |
  
  g4-> g |
  g\< a8[ g] |
  fis([\! a] d4--)~ |
  d d,4\mf |
  a'4-> a |
  a b8[ a] |
  g(([ b] d4)~ |
  d d4\cresc |
  e e |
  
  \once\override DynamicText #'self-alignment-X = #4
  \once\override DynamicLineSpanner #'extra-offset = #'( 0 . 4 )
  g4.\f-> d8 |
  c4 d8[ c] |
  b4 g8\cresc[ a]\! |
  b4 a8[ g] |
  a4 g8[ e] |
  g8. d'16\ff d8. d16 |
  g2~ |
  g8 b,\rest\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = \markup\dynamic"ff  "
  Ta -- ran -- ta -- ra,
  Ta -- ran -- ta -- ra,
  Ta -- ran -- ta -- ra,
  Ta -- ran -- ta -- ra __
  \set stanza = #"1. "
  For horse and hound the horn doth sound,
  Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, ta -- ra. __
  
  The horn doth sound
  For horse and hound,
  Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, ta -- ra.
  
  So the dogs be -- gin to bark and bay,
  \set ignoreMelismata = ##t
  And the \unset ignoreMelismata hors -- es __ am -- ble a -- long the way,
  While the red -- coats \set ignoreMelismata = ##t mus -- ter in strong ar -- _ ray,
  They mus -- ter in strong _ ar -- ray,
  \unset ignoreMelismata
  Ta -- ran -- ta -- ra. __ A --
  
  hunt -- ing we will go, __
  A -- hunt -- ing we will go, __
  Through ma -- ny~a co -- zy cov -- ert,
  For the scent is keen I trow, __
  A -- hunt -- ing we will go, __
  A -- hunt -- ing we will go, __
  With horse and hound, where game is found,
  A -- hunt -- ing we will go,
  Ta -- ran -- ta -- ra. __
}

sopWordsII = \lyricmode {
  \repeat unfold 16 \skip1
  \set stanza = #"2. "
  The fox is found, the horn doth sound,
  Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, ta -- ra. __
  
  The horn doth sound, \set ignoreMelismata = ##t
  For the fox is found, \unset ignoreMelismata
  Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, Ta -- ran -- ta -- ra, ta -- ra.
  
  \set ignoreMelismata = ##t
  To be in at the death \unset ignoreMelismata the hunt -- ers ride,
  And skim \set ignoreMelismata = ##t like the wind \unset ignoreMelismata o’er the coun -- try side,
  For the brush \set ignoreMelismata = ##t is the Queen of _ Beau -- ty’s _ pride,
  The Queen of _ Beau -- _ ty’s pride,
  Ta -- ran -- ta -- ra. __ _ A --
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d'8 d4 e8 |
  d4 a8 d4 e8 |
  d4 d,8 d4 e8 |
  d4 a8 d4 e8 |
  
  fis4.~ fis~ |
  fis4 s8 s4 d8 |
  b4. b |
  b b |
  d g4( fis8) |
  g4 g8 g4 g8 |
  
  fis4 fis8 fis4 fis8 |
  g4 g8 g4 g8 |
  g4. e |
  d~ d8 s d |
  
  %page2/61
  d4( b8) b4( d8) |
  d4( b8) \slurDashed b4( d8) |
  \slurSolid d4. g |
  g4 g8 g4 g8 |
  g4 g8 g4 g8 |
  
  fis4 fis8 fis4 fis8 |
  g4. g |
  fis d4 d8 |
  d4. \tieDashed e4~ e8 |
  \tieSolid d4. d |
  
  e e |
  d \tieDashed d4~ d8 |
  fis4. fis4~ fis8 |
  \tieSolid fis4. fis4 fis8 |
  g4. g4( e8) |
  
  %page3/62
  d4. e4 e8 |
  d4. \tieDashed fis4~ fis8 |
  \tieSolid \slurDashed fis4. fis4( b8) |
  \slurSolid ais4. ais4( fis8) |
  fis4. eis |
  
  fis \tieDashed d4~ d8 |
  \tieSolid cis4.~ cis4 cis8 |
  d4 d8 d4 e8 |
  d4.~ d8 d4 |
  
  \time 2/4
  d d |
  d d |
  d8([ e] fis4)~ |
  fis fis |
  d fis |
  d fis |
  g2~ |
  g4 g |
  g4. g8 |
  
  %page4/63
  g4 g |
  g g |
  g4. g8 |
  g4 g |
  g e |
  d8[( e] fis4)~ |
  fis d |
  
  d d |
  d d |
  d8([ e] fis4)~ |
  fis fis |
  d fis |
  d fis |
  g2~ |
  g4 g |
  g g |
  
  g4. g8 g4 fis |
  g g8[ a] |
  b4 a8[ g] |
  a4 g8[ e] |
  g8. d16 d8. d16 |
  b'2~ |
  b8 s \bar"|."
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
  d8 d4 e8 |
  d4 a8 d4 e8 |
  d4 d,8 d4 e8 |
  d4 a'8 a4 a8 |
  
  a4.~ a~ |
  a4 s8 s4 d,8 |
  b'4. g |
  b g |
  b d |
  d4 d8 d4 d8 |
  
  d4 d8 d4 d8 |
  d4 d8 b4 d8 |
  cis4. cis |
  c?~ c8 s c |
  
  %page2/61
  b4. g |
  b \tieDashed g4~ g8 |
  b4. d4( e8) |
  d4 d8 d4 d8 |
  d4 d8 d4 d8 |
  
  d4 d8 d4 d8 |
  cis4. cis |
  d d,4 d8 |
  a'4. a4~ a8 |
  a4. c |
  
  b b |
  b b4~ b8 |
  a4( gis8) \slurDashed a4( b8) |
  \slurSolid c4( e8) d4 d8 |
  d4. b |
  
  %page3/62
  b4. b4 b8 |
  b4. d4~ d8 |
  b4. d4~ d8 |
  cis4. e4( cis8) |
  b4. b |
  
  d \slurDashed d4( a8) |
  \tieSolid g4.~ g4 g8 |
  \slurSolid fis4 d'8 d4 e8 |
  d4.~ d8 d4 |
  
  \time 2/4
  b4-> b4 |
  b b |
  c2~ |
  c4 c |
  c-> c |
  c c |
  b2~ |
  b4 d |
  c4.-> e8 |
  
  %page4/63
  e4 c |
  c-> d |
  d4. d8 |
  cis4-> cis |
  cis cis |
  c?2~ |
  c4 d8[ c] |
  
  b4-> b |
  b b |
  c2~ |
  c4 c |
  c-> c |
  c c |
  b2~ |
  b4 d |
  c e |
  
  d4.-> d8 |
  e4 d |
  d g,8[ a] |
  b4 a8[ g] |
  a4 g8[ e] |
  g8. d'16 d8. d16 |
  d2~ |
  d8 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d8 d4 e8 |
  d4 a8 d4 e8 |
  d4 d,8 d4 e8 |
  d4 a8 d4 e8 |
  
  d4.~ d~ |
  d4 d8\rest d4\rest\fermata d8 |
  g4. e4( d8) |
  g4. e4( d8) |
  g4. d |
  b'4 b8 b4 b8 |
  
  c4 c8 c4 c8 |
  b4 g8 g4 d8 |
  e4. g |
  fis( e8) d\rest d |
  
  %page2/61
  g4. e4( d8) |
  \slurDashed g4. e4( d8) |
  g4. g |
  b4 g8 g4 g8 |
  b4 b8 bes4 bes8 |
  
  a4 a8 a4 a8 |
  a4. a |
  d, d4 d8 |
  fis4. \tieDashed d4~ d8 |
  fis4. d |
  
  g4. d |
  g d4~ d8 |
  d'4. d,4~ d8 |
  d'4. d,4 d8 |
  g4. d |
  
  %page3/62
  g4. e4 e8 |
  fis4. fis4~ fis8 |
  fis4. fis4~ fis8 |
  fis4. fis |
  b gis |
  
  a fis4( d8) |
  \tieSolid a4.~ a4 a8 |
  d4 \slurSolid d8 d4 e8 |
  d4.~ d8\fermata d4 |
  
  \time 2/4
  g4 d |
  g d |
  a'4( d, |
  a') d, |
  fis d |
  fis d |
  g( d |
  b') b |
  c4. c8 |
  
  %page4/63
  c4 c |
  c b |
  g4. g8 |
  e4 e |
  e a |
  d,2~ |
  d4 d |
  
  g d |
  g d |
  a'( d, |
  a') d, |
  fis d |
  fis d |
  g( d |
  b') b |
  c c |
  
  b4. b8 |
  a4 d, |
  g g8[ a] |
  b4 a8[ g] |
  a4 g8[ e] |
  g8. d'16 d8. d16 |
  g,2~ |
  g8 d8\rest\fermata \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"With Horse and Hound"}}
  poet = \markup\oldStyleNum"H. L. D’arcy Jaxone (d. 1915)"
  composer = \markup\oldStyleNum"Alfred J. Caldicott (1842–1897)"
  tagline = ""
}}


global = {
  \key g \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	g'4\p\< g g |
  b4.\! a8\> g4 |
  b\! b\< b |
  d4.\! c8\> b4 |
  d\!\f c\dim b\! |
  
  a2 b4\rest |
  a2\p g8 fis |
  g4 a b |
  c2\cresc b8\! a |
  b4 c d |
  \times 2/3 { e8[^\markup{\dynamic f \italic"rit. e dim."} d c] } b4 a |
  g2\fermata b4\rest \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	If I a bird -- ling were,
  And with two wings could fly,
  I’d fly to thee;
  
  But, as no wings are mine,
  But, as no wings are mine,
  That can -- not be.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Though far a -- way from thee,
  Dream -- ing I’m e’er with thee,
  Whis -- p’ring to thee;
  
  But, when I wake at last,
  But, when I wake at last,
  Then I’m a -- lone.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  There is no hour at night
  When thy dear im -- age bright
  Strays from my heart.
  
  Thou’st said ten thou -- sand times,
  Thou’st said ten thou -- sand times,
  That mine thou art.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 d d |
  g4. fis8 g4 |
  g g g |
  b4. a8 g4 |
  g fis g |
  
  fis2 s4 |
  d2 d8 d |
  d4 fis g |
  g2 g8 g |
  g4 g g |
  a g fis |
  d2 s4 \bar "|."
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
  b4 b b |
  d4. c8 b4 |
  d d d |
  d4. d8 d4 |
  d d d |
  
  d2 s4 |
  c2 b8 a |
  g4 d' d |
  e2 d8 c |
  d4 d d |
  e d c |
  b2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 g g |
  g4. g8 g4 |
  g g g |
  g4. g8 g4 |
  b4 a g |
  
  d2 d4\rest |
  d2 d8 c |
  b4 a g |
  g'2 g8 g |
  g4 a b |
  c4 d d, |
  g2\fermata d4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Flight of Love"}}
  composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}}


