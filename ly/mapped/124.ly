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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #124
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
  \tieDashed
}

sopMusic = \relative c' {
  \partial 4
  \repeat volta 2{
    g'4 |
    g g a g8( a) |
    b2 b4\rest b8~ b |
    b4 a8( g) a4 b8 c |
    b2 b4\rest g8 g |
    
    e4 g c d8 e |
    d4. b8 g4~ g8 g |
    g4 bes8 bes cis4 e |
    d2. d8~ d |
    a4 c b\cresc d8\! d |
    
    a4 c b( e8) e |
    a,4 a8~ a a4 b8[ cis] |
    d2 b4\rest a8\p a |
    a4 gis8( a) cis4 b8( a) |
    a2. fis4 |
    
    g4 fis8 g b4 a8[ g] |
    fis2 b4\rest a8~ a |
    d4 d8~\cresc d\! d4 c8 b |
    c4 c8 c c4 b8 a |
    
    b4 a8( g) g4^\markup\italic"rit." fis8[ g] |
    a2. b4\rest^\markup\italic"a tempo" |
    b8 ais b4 b d, |
    a'2 b4\rest d, |
    a'4 a8 b c4 b8[ a] |
    
    b2. g4 |
    e'4\cresc fis8\~ e d4 g, |
    c8 c d[ c] b2 |
    b4\f a8^\markup\italic"rall." g a4. g8 |
    g2.
  }
  e4 |
  
  e e g\cresc c8\! c |
  e2. b4\rest |
  e4 d8 c g'4. d8 |
  e2. c8\f c |
  f4. f8 e4 e8 e |
  
  dis4. dis8 e4 b8\rest e |
  e4 dis8 cis dis4. b8 |
  e2. b8 b |
  b4\dim b\! b a8 b |
  c2. c4 |
  
  b b b a8[ b] |
  c2. c8 c |
  b4^\markup\italic"rit." b b b8 b |
  b4. b8 b4 b8 b |
  b4 fis g e |
  b'1 |
  
  b8^\markup{\dynamic"p" \italic"Slower"} ais8 b4 b d, |
  a'2. d,4 |
  a' a8 b c4 b8[ a] |
  b2. g4 |
  e'4 fis8 e d4 g, |
  c d8[ c] b2 |
  
  b4 a8 g b4 a8[ g] |
  a2. b4\rest |
  b8 ais b4 b d |
  a2. d,4 |
  a'4 a8 b c4 b8[ a] |
  
  b2. g4\cresc |
  e'8[\! g] fis^\markup\italic"rit." e d4 g, |
  c8[ e] d[ c] b2 |
  b2\dim a4\! g |
  a2. g4 |
  g1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	A maid -- en sat at her door,
  And _ sighed as she looked at the sea;
  “I’ve a dear, dear love, on a dis -- tant shore, _
  A -- dy -- ing for news of me,
  I’ve a dear, dear love, on a dis -- tant shore, _
  A -- dy -- ing for news of _ me.”
  And the wind was _ lis -- ten -- ing near,
  And saw that the maid was _ fair,
  So the kind wind _ whis -- pered a hope in her ear,
  As he played with her bright brown _ hair:
  “Be of good cheer, sweet heart,
  I fly to that dis -- tant _ shore,
  Thy lov -- er I’ll tell thou lov -- est him _ well,
  Ev -- er and ev -- er more.”
  
  \set stanza = #" 3."
  \unset ignoreMelismata
  The wind tore o -- ver the wave,
  Scat -- ter -- ing o -- cean spray,
  But a -- lack! the lov -- er he flew to save,
  He met on his home -- ward way,
  And his good ship sank in the gale,
  And ev -- ’ry soul be -- side,
  And the wind came sob -- bing to tell the tale,
  And the maid -- en drooped and died.
  Be of good cheer, poor heart,
  At rest on a dis -- tant shore,
  Where thou and thy love walk hand in hand,
  Ev -- er and ev -- er more!
  Be of good cheer, dear heart,
  At rest on a dis -- tant shore,
  Where thou and thy love go hand in hand,
  Ev -- er and ev -- er more!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  The maid -- en dried her _ eyes,
  And a smile shone _ o -- ver her face,
  For she saw bright hope in the chang -- ing skies,
  As the wind flew _ off a -- pace,
  She _ saw bright hope in the chang -- ing skies,
  As the wind flew _ off a -- _ pace.
  And she bade the kind wind good _ speed, ""
  “Hur -- ry, O wind,” said _ she,
  “Oh, _ say that I love him in -- deed, and in -- deed.”
  And the wind cried _ o -- ver the sea,
  “Be of good cheer, sweet heart,
  I fly to that dis -- tant _ shore,
  Thy lov -- er I’ll tell thou % lov -- est him _ well,
  % Ev -- er and ev -- er more.”
  
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
  d4 |
  d d fis e8( fis) |
  g2 s4 g8~ g |
  g4 fis8( e) d4 d8 d |
  d2 s4 d8 d |
  
  c4 e g g8 g |
  g4. g8 d4~ d8 d |
  e4 e8 e g4 g |
  g2. g8~ g |
  fis4 a g g8 g |
  
  fis4 a g~ g8 g |
  fis4 fis8~ fis e4 g |
  fis2 s4 fis8 fis |
  g4 e8~ e g4 g8~ g |
  fis2. d4 |
  
  cis4 cis8 cis cis4 e |
  d2 s4 fis8~ fis |
  g4 g8~ g g4 g8 g |
  e4 g8 g fis4 fis8 fis |
  
  g4 d8~ d e4 dis8( e) |
  fis2. s4 |
  g8 fis g4 g d |
  fis2 s4 d |
  fis fis8 fis fis4 fis |
  
  g2. g4 |
  g g8 g g4 g |
  g8 g fis4 g2 |
  g4 e8 e fis4. d8 |
  d2.
  
  
  c4 |
  
  c4 c e e8 e |
  g2. s4 |
  g g8 g b4. g8 |
  g2. e8 e |
  a4. a8 g4 g8 g |
  
  fis4. fis8 g4 s8 g |
  a4 a8 a a4. a8 |
  gis2. e8 e |
  e4 e e e8 e |
  e2. e4 |
  
  e e e e |
  e2. e8 e |
  e4 e dis dis8 dis |
  fis4. fis8 e4 e8 e |
  dis4 dis e e |
  dis1 |
  
  d?8 cis d4 d b |
  d2. d4 |
  fis fis8 g a4 g8[ fis] |
  g2. g4 |
  g g8 g g4 g |
  g fis g2 |
  
  g4 fis8 e e4 e |
  fis2. s4 |
  g8 fis g4 g g |
  fis2. d4 |
  fis fis8 g a4 g8[ fis] |
  
  g2. g4 |
  g4 g8 g g4 g |
  g fis g2 |
  g e4 e |
  fis2. d4 |
  g1 \bar"|."
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
  b4 |
  b b d d8~ d |
  d2 s4 d8~ d |
  e4 b8~ b fis4 g8 a |
  g2 s4 b8 b |
  
  c4 c c b8 c |
  b4. d8 b4~ b8 b |
  bes4 cis8~ cis bes4 bes |
  b?2. b8~ b |
  d4 d d b8 b |
  
  d4 d d( b8) b |
  d4 d8~ d cis4 d8[ e] |
  d2 s4 d8 d |
  cis4 cis8~ cis e4 d8[ cis] |
  d2. a4 |
  
  a4 a8 a a4 a |
  a2 s4 d8~ d |
  b4 b8~ b b4 b8 b |
  a4 e'8 e d4 d8 d |
  
  d4 c8( b) b4 b8~ b |
  d2. s4 |
  d8 cis d4 d b4 |
  d2 s4 fis, |
  c' c8 b a4 d |
  
  d2. b4 |
  c d8 c b4 b |
  a8 a a[ d] d2 |
  e4 b8 b c4. b8 |
  b2.
  
  
  g4 |
  
  g g c g8 g |
  c2. s4 |
  b4 b8 b d4. b8 |
  c2. g8 g |
  c4. c8 c4 c8 c |
  
  c4. c8 c4 s8 c |
  fis,4 fis8 fis b4. b8 |
  b2. gis8 gis 
  gis4 gis gis gis8 gis |
  a2. a4 |
  
  gis4 gis gis gis |
  a2. a8 a |
  g4 g a a8 a |
  a4. a8 g4 g8 g |
  a4 b b g |
  fis1 |
  
  g8 g g4 g g |
  fis2. fis4 |
  d'4 d8 d d4 d |
  d2. b4 |
  c4 c8 c b4 b |
  a4 a8[ d] d2 |
  
  e4 b8 b cis4 cis |
  d2. s4 |
  d8 cis d4 d b |
  d2. fis,4 |
  d'4 d8 d d4 d |
  
  d2. b4 c8[ e] d8 c b4 b |
  c4 d d2 |
  e2 b4 b |
  c2. b4 |
  b1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 |
  g g d d8~ d |
  g2 d4\rest g8~ g |
  e4 e8~ e d4 d8 d |
  g,2 d'4\rest g8 g |
  
  c,4 c e d8 c |
  g'4. g8 g4~ g8 g |
  g4 g8~ g g4 g |
  g2. g8~ g |
  d4 fis g g8 g |
  
  d4 fis g~ g8 g |
  a4 a8~ a a4 a |
  d,2 d4\rest d8 d |
  a'4 a8~ a a4 a |
  d,2. d4 |
  
  e4 e8 e a,4 cis |
  d2 d4\rest d8~ d |
  g4 g8~ g e4 e8 e |
  a4 a8 a d,4 d8 d |
  
  g4 g8~ g e4 e8~ e |
  d2. d4\rest |
  g8 g g4 g g |
  d2 d4\rest d4 |
  d d8 d d4 d |
  
  g2. g4 |
  c, c8 c g'4 g |
  e8 e d4 g2 |
  e4 e8 e d4. d8 |
  g2.
  
  
  c,4 |
  
  c4 c c c8 c |
  c2. d4\rest |
  g4 g8 g g4. g8 |
  c,2. c8 c |
  c4. c8 c4 c8 c |
  
  c4. c8 c4 d8\rest c |
  b4 b8 b fis'4. fis8 |
  e2. e8 e |
  e4 e e e8 e |
  a,2. a4 |
  
  e' e e e |
  a,2. a8 a |
  e'4 e fis fis8 fis |
  dis4. dis8 e4 e8 e |
  fis4 b, e e |
  b1 |
  
  g8 g g4 g g |
  d'2. d4 |
  d d8 d fis4 d |
  g2. g4 |
  c,4 e8 e g4 g |
  e d g2 |
  
  e4 e8 e a4 a |
  d,2. d4\rest |
  g8 g g4 g g |
  d2. d4 |
  d d8 d d4 d |
  
  g2. g4 |
  c, c8 c g'4 g |
  a d, g2 |
  e e4 e |
  d2. d4 |
  g1 \bar"|."
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
      \override LyricText #'font-size = #1.1
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Distant Shore"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"Arthur Sullivan (1842–1900)"
  tagline = ""
}}


