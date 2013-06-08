\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Annie Laurie"}}
  composer = \markup\oldStyleNum"Lady John Scott (1810–1900)"
  poet = \markup\oldStyleNum"William Douglas (c. 1672–1748)"
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
       (padding . -1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 70))
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
  first-page-number = #196
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
  \key c \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4
	e8[ d] |
  c4. c8 c'4. c8 |
  b4 a b\rest a |
  g4 e e d8[ c] |
  \acciaccatura e8 d2 b'4\rest e,8[ d] |
  
  c4. c8 c'4. c8 |
  b4 a b\rest a |
  g4. e8 e4. d8 |
  c2 b'4\rest g |
  c4. c8 d4. d8 |
  e2 b4\rest g4 |
  
  c4. c8 d4. d8 |
  e2 e4. d8 |
  c4. b8 a4 c8[ a] |
  g4 e2 e8[ d] |
  c( c'4) e,8 e4. d8 |
  c2 b'4\rest \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Max -- wel -- ton braes are bon -- nie,
  Where ear -- ly fa’s the dew,
  \set ignoreMelismata = ##t
  And it’s there that An -- nie Lau -- rie,
  \unset ignoreMelismata
  Gie’d me her prom -- ise true,
  Gie’d me her prom -- ise true,
  Which ne’er for -- got will be;
  And for bon -- nie An -- nie Lau -- rie,
  I’d lay me down and dee.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Her brow is like the snaw -- drift
  Her throat is like the swan,
  Her face it is the fair -- est,
  That e’er the sun shone on,
  That e’er the sun shone on;
  And dark blue is her e’e,
  And for bon -- nie An -- nie Lau -- rie,
  I’d lay me down and dee.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Like dew on~the gow -- an ly -- ing
  Is~the fa’ o’~her fair -- y __ feet,
  \set ignoreMelismata = ##t
  Like the winds in sum -- mer sigh -- ing,
  \unset ignoreMelismata
  Her voice is low and sweet,
  Her voice is low and sweet;
  She’s a’ the world to me,
  And for bon -- nie An -- nie Lau -- rie,
  I’d lay me down and dee.
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  c8[ b] |
  c4. c8 e4. g8 |
  g4 f4 s f |
  e c c b8[ c] |
  b2 s4 c8[ b] |
  
  c4. c8 e4. g8 |
  g4 f s f |
  e4. c8 c4. b8 |
  c2 s4 e4 |
  e4. e8 g4. g8 |
  g2 s4 g8[ f] |
  
  e4. g8 g4. g8 |
  g2 g4. f8 |
  e4. f8 f4 f |
  e4 c2 c8[ b] |
  c8( e4) c8 c4. b8 |
  c2 s4 \bar"|."
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
  \partial 4
  g8[ f] |
  e4. e8 g4. g8 |
  c4 c s c |
  c g g f8[ e] |
  g2 s4 g8[ f] |
  
  e4. e8 g4. g8 |
  c4 c s c |
  c4. g8 g4. f8 |
  e2 s4 c' |
  g4. g8 b4. b8 |
  c2 s4 d |
  
  c4. c8 b4. b8 |
  c2 c4. b8 |
  c4. c8 c4 a |
  c4 g2 g8[ f] |
  g4. g8 g4. f8 |
  e2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  c,4 |
  c4. c8 c4. e8 |
  f4 f d\rest f |
  g g, g g |
  g2 d'4\rest g,8[ g] |
  
  c4. c8 c4. e8 |
  f4 f d\rest f4 |
  g4. g,8 g4. g8 |
  c2 d4\rest c |
  c4. c8 g'4. g8 |
  c,2 d4\rest g4 |
  
  c,4. e8 g4. g8 |
  c,2 g'4. g8 |
  a4. f8 f4 f |
  c4 c2 c4 |
  e8( c4) g8 g4. g8 |
  c2 d4\rest \bar"|."
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


