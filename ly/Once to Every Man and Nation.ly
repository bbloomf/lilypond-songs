\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Once to Every Man and Nation"}}
  poet = \markup\oldStyleNum"James Russell Lowell (1819–1891)"
  composer = \markup\oldStyleNum"Thomas John Williams (1869–1944)"
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
       (padding . 1)
       (stretchability . 100))
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
  \key aes \major
  \time 4/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f2 \times 2/3 {f4( g aes)} g2 f |
  g \times 2/3 {g4( aes bes)} aes4.( g8) f2 |
  c' \times 2/3 {bes4( c des)} c4.( bes8) aes2 |
  bes4.( aes8) g2 f1 |
  
  f2 \times 2/3 {f4( g aes)} g2 f |
  g \times 2/3 {g4( aes bes)} aes4.( g8) f2 |
  c' \times 2/3 {bes4( c des)} c4.( bes8) aes2 |
  bes4.( aes8) g2 f1 |
  
  c'2 \times 2/3 {aes4( bes c)} bes2 bes |
  aes \times 2/3 {f4( g aes)} g2 g |
  f2 \times 2/3 {f4( g aes)} bes2 bes |
  aes \times 2/3 {bes4( aes bes)} c1 |
  
  f,2 \times 2/3 {f4( g aes)} g2 f |
  g \times 2/3 {g4( aes bes)} aes4.( g8) f2 |
  c' \times 2/3 {bes4( c des)} c4.( bes8) aes2 |
  bes4.( aes8) g2 f1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Once to ev -- ’ry man and na -- tion
  Comes the mo -- ment to de -- cide,
  In the strife of truth with false -- hood,
  For the good or e -- vil side;
  Some great cause, some great de -- ci -- sion,
  Of -- f’ring each the bloom or blight,
  And the choice goes by for -- ev -- er
  ’Twixt that dark -- ness and that light.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
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
  c2 c e f |
  f e f c |
  c \times 2/3 {ees?2( f4)} g2 f |
  f e f1 |
  
  c2 c e f |
  f e f c |
  c \times 2/3 {ees?2( f4)} g2 f |
  f e f1 |
  
  ees2 ees ees ees |
  c f f e |
  f c f ees |
  c f e1 |
  
  aes,2 f' e f |
  f e f f |
  ees? ees ees f |
  f e f1 \bar"|."
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
  aes2 c c aes |
  des c c4.( bes8) aes2 |
  ees' \times 2/3 {bes2( aes4)} c2 c |
  des4.( c8) bes2 aes1 |
  
  aes2 c c aes |
  des c c4.( bes8) aes2 |
  ees' \times 2/3 {bes2( aes4)} c2 c |
  des4.( c8) bes2 aes1 |
  
  aes2 aes aes g |
  aes c c c |
  f, aes aes g |
  aes f g1 |
  
  f2 c' c aes |
  des c c4.( bes8) aes2 |
  aes \times 2/3 {g4( aes bes)} aes4.( g8) f2 |
  des'4.( c8) bes2 aes1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,2 aes, c des |
  bes c f f |
  aes \times 2/3 {g2( f4)} e2 f |
  bes, c f1 |
  
  f,2 aes c des |
  bes c f f |
  aes \times 2/3 {g2( f4)} e2 f |
  bes, c f1 |
  
  aes,2 c ees ees |
  f aes, c c |
  f ees des ees |
  f4( ees) des2 c1 |
  
  des2 aes c des |
  bes c f f |
  aes ees aes, des |
  bes c f1 \bar"|."
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
    \tempo 4 = 180
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
