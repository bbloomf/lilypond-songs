\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"When You and I Were Young, Maggie"}}
  poet = \markup\oldStyleNum"George W. Johnson (d. 1917)"
  composer = \markup\oldStyleNum"James Austin Butterfield (1837–1891)"
  tagline = ""
}
\paper {
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
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 100))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #68
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
	\partial 4
  c'4 |
  c a8. g16 f4 g8. f16 |
  f2 d8 f4 d8 |
  c4 f8.~ f16 a4 c8.~ c16 |
  
  g2. \bar""\break c4 |
  c a8. g16 f4 g8. f16 |
  f2 d8 f4 d8 |
  c4 f8.( a16) c4 g8.( a16) |
  f2. \bar""
  
  
    f4 |
    d'4 d8. d16 bes4 d8. d16 |
    c2 a8 c4 a8 |
    g4 c8.~ c16 c8[ b] e8( d) |
    c2. \bar""
    
    c8.~ c16 |
    c4 a8. g16 f4 g8.( f16) |
    f2 d8 f4 d8 |
    c4 f8.[ a16] c4 g8.[ a16] |
    f2. \bar""
  
    
    f4 |
    d'4 d8. d16 bes4 d8. d16 |
    c2 a8 c a8. a16 |
    g4 c8 c c8[ b] e8 d |
    c2 b4\rest \bar""
    
    c8 c |
    c4 a8. g16 f4 g8 f |
    f2 d8 f4\fermata d8 |
    c4 f8.[ a16] c4 g8.[ a16] |
    f2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	I wan -- dered to day to the hill, Mag -- gie,
  To watch the __ _ scene be -- _ low,
  The creek and the creak -- ing old mill, Mag -- gie,
  As we used to long, a -- _ go.
  
  The green grove is gone from the hill, Mag -- gie,
  Where first the __ _ dai -- _ sies __ _ \set associatedVoice = "altos" sprung; _ _
  \unset associatedVoice
  The _ creak -- ing old mill is __ _ still, Mag -- gie,
  Since you and _ I were _ young.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  A cit -- y so si -- lent and lone, Mag -- gie,
  Where young and the gay and the best,
  In pol -- ished white man -- sions of stone, Mag -- gie,
  Have each found a place of __ _ rest,
  
  Is built where the birds used to play, Mag -- gie,
  And join in the songs _ that were \set associatedVoice = "altos" sung, __ _ _
  \unset associatedVoice
  For we sang just as gay as __ _ they, Mag -- gie,
  When you and _ I were _ young.
  
  And now we are a -- ged and gray, Mag -- gie,
  And the tri -- als of life _ near -- ly \set associatedVoice = "altos" done.
  \unset associatedVoice
  Let us sing of the days that are gone, Mag -- gie,
  When you and _ I were _ young.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  They say I am fee -- ble with age, Mag -- gie,
  My steps are less spright -- ly than then;
  My face is a well -- writ -- ten page, Mag -- gie,
  But time a -- _ lone was the pen.
  
  They say we are a -- ged and gray, Mag -- gie,
  As sprays by the white _ break -- ers \set associatedVoice = "altos" flung, __ _ _
  \unset associatedVoice
  But to me you’re as fair as you were, Mag -- gie,
  When you and _ I were _ young.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  a'4 |
  a f8. f16 ees4 c8. c16 |
  d2 bes8 d4 bes8 |
  c4 f8.~ f16 f4 f8.~ f16 |
  
  e2. e4 |
  f4 f8. f16 f4 ees8. ees16 |
  d2 bes8 d4 bes8 |
  c4 c8.( f16) e4 e8.~ e16 |
  f2. \bar""
  
    f4 |
    f4 f8. f16 f4 f8. f16 |
    f2 f8 f4 f8 |
    e4 g8.~ g16 f4 g8( f) |
    \slurSolid e4( f g) \bar""
    
    \slurDashed e8.~ e16 |
    f4 f8. f16 f4 ees8.~ ees16 |
    d2 bes8 d4 d8 |
    c4 c8.[ f16] e4 e |
    f2. \bar""\break
    
    
    f4 |
    f4 f8. f16 f4 f8. f16 |
    f2 f8 f f8. f16 |
    e4 e8 e e[ d] g f |
    e2 s4 \bar""
    
    f8 e |
    f4 f8. e16 f4 c8 c |
    d2 bes8 d4 bes8 |
    a4 c e c |
    c2. \bar"|."
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
  f4 |
  f c8. bes16 a4 bes8. a16 |
  bes2 f8 bes4 f8 |
  a4 a8.~ a16 c4 a8.~ a16 |
  
  c2. bes4 |
  a c8. bes16 a4 a8. a16 |
  bes2 f8 bes4 f8 |
  a4 a8.( c16) bes4 bes8.( c16) |
  a2. \bar""
  
    a4 |
    bes bes8. bes16 bes4 bes8. bes16 |
    a2 c8 a4 c8 |
    c4 e8.~ e16 d4 b8~ b |
    \slurSolid g4( a bes) \bar""
    
    bes8.~ bes16 |
    a4 c8. bes16 a4 a8.~ a16 |
    bes2 f8 bes4 bes8 |
    a4 a8.[ c16] bes4 bes8.[ c16] |
    a2. \bar""
    
    
    a4 |
    bes bes8. bes16 d4 bes8. bes16 |
    a2 c8 c c8. c16 |
    c4 c8 c g4 b8 b |
    c2 s4 \bar""
    
    c8 bes |
    a4 c8. bes16 a4 a8 a |
    bes2 f8 bes4 f8 |
    f4 a8.[ f16] g4 bes8.[ c16] |
    a2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 |
  f f8. f16 f4 f8. f16 |
  bes,2 bes8 bes4 bes8 |
  f'4 f8.~ f16 f4 f8.~ f16 |
  
  c2. c4 |
  f f8. f16 f4 f8. f16 |
  bes,2 bes8 bes4 bes8 |
  c4 c8.( f16) g4 c,8.~ c16 |
  f2. \bar""
  
    f4 |
    bes,4 bes8. bes16 d4 bes8. bes16 |
    f'2 f8 f4 f8 |
    g4 g8.~ g16 g4 g8~ g |
    c,2. \bar""
    
    g'8.~ g16 |
    f4 f8. f16 f4 f8.~ f16 |
    bes,2 bes8 bes4 bes8 |
    c4 c g' c, |
    f2.
    
    
    f4 |
    bes,4 bes8. bes16 bes4 bes8. bes16 |
    f'2 f8 f f8. f16 |
    g4 g8 g g,4 g8 g |
    c4 c8 c c'[ bes] \bar""
    
    a g |
    f4 f8. f16 f4 f8 f |
    bes,2 bes8 bes4\fermata bes8 |
    c4 c c c |
    <f f,>2. \bar"|."
}
bassWords = \lyricmode {
  \repeat unfold 82 \skip1
  Let us sing,
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
    \new Lyrics = "basses"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "basses"  \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 100
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}


